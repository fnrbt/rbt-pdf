#!/usr/bin/env python3
"""Generate small ICC input profiles and independent LittleCMS sRGB oracles.

Run: python3 tests/render-corpus/icc_profiles.py /tmp/rbt-icc-fixtures
No package dependencies. LittleCMS 2 is loaded via ctypes if installed.
The output JSON also records the PDF /N, sample bytes, and recommended PDF
/ColorSpace and /Decode checks. Profiles are uncompressed stream bytes.
"""
import ctypes
import ctypes.util
import json
import struct
import sys
from pathlib import Path

D50 = (0.9642, 1.0, 0.8249)


def u16(x):
    return struct.pack('>H', x)


def u32(x):
    return struct.pack('>I', x)


def s15(x):
    return struct.pack('>i', round(x * 65536))


def xyz(x, y, z):
    return b'XYZ ' + b'\0' * 4 + b''.join(s15(v) for v in (x, y, z))


def curve_linear():
    # ICC curv count 0 is the identity response curve.
    return b'curv' + b'\0' * 4 + u32(0)


def profile(space, pcs, tags):
    assert len(space) == len(pcs) == 4
    count = len(tags)
    data = bytearray(132 + 12 * count)
    entries = []
    for name, payload in tags:
        assert len(name) == 4
        while len(data) % 4:
            data.append(0)
        offset = len(data)
        data.extend(payload)
        entries.append((name, offset, len(payload)))
    data[:4] = u32(len(data))
    data[8:12] = u32(0x02200000)  # v2.2 ICC profile
    data[12:16] = b'scnr'        # input/scanner class
    data[16:20] = space.encode('ascii')
    data[20:24] = pcs.encode('ascii')
    data[36:40] = b'acsp'
    data[68:80] = b''.join(s15(v) for v in D50)
    data[128:132] = u32(count)
    for i, (name, offset, length) in enumerate(entries):
        p = 132 + i * 12
        data[p:p + 12] = name.encode('ascii') + u32(offset) + u32(length)
    return bytes(data)


def gray():
    return profile('GRAY', 'XYZ ', [('wtpt', xyz(*D50)), ('kTRC', curve_linear())])


def swapped_rgb():
    # D50-adapted sRGB primaries, intentionally exchange red and green.
    red = (0.4361, 0.2225, 0.0139)
    green = (0.3851, 0.7169, 0.0971)
    blue = (0.1431, 0.0606, 0.7141)
    return profile('RGB ', 'XYZ ', [
        ('wtpt', xyz(*D50)), ('rXYZ', xyz(*green)),
        ('gXYZ', xyz(*red)), ('bXYZ', xyz(*blue)),
        ('rTRC', curve_linear()), ('gTRC', curve_linear()),
        ('bTRC', curve_linear()),
    ])


def linear_rgb():
    return profile('RGB ', 'XYZ ', [
        ('wtpt', xyz(*D50)), ('rXYZ', xyz(0.4361, 0.2225, 0.0139)),
        ('gXYZ', xyz(0.3851, 0.7169, 0.0971)),
        ('bXYZ', xyz(0.1431, 0.0606, 0.7141)),
        ('rTRC', curve_linear()), ('gTRC', curve_linear()),
        ('bTRC', curve_linear()),
    ])


def adobe_rgb():
    # Adobe RGB (1998), D50-adapted primaries and the exact 563/256 TRC.
    gamma = b'curv' + b'\0' * 4 + u32(1) + u16(563)
    return profile('RGB ', 'XYZ ', [
        ('wtpt', xyz(*D50)), ('rXYZ', xyz(0.6097559, 0.3111242, 0.0194811)),
        ('gXYZ', xyz(0.2052401, 0.6256560, 0.0608902)),
        ('bXYZ', xyz(0.1492240, 0.0632197, 0.7448387)),
        ('rTRC', gamma), ('gTRC', gamma), ('bTRC', gamma),
    ])


def lut_header(kind):
    # Identity 3x3 matrix is ignored for CMYK input (4 channels).
    tag = bytearray(kind + b'\0' * 4 + bytes((4, 3, 2, 0)))
    tag.extend(b''.join(s15(1.0 if r == c else 0.0)
                        for r in range(3) for c in range(3)))
    assert len(tag) == 48
    return tag


def cmyk_lut(bits):
    # Deliberately unusual but valid input profile: K=0 maps to Lab black,
    # K=1 maps to Lab white, independent of C/M/Y. 3D/4D interpolation must
    # be honored: it cannot accidentally pass as naive DeviceCMYK.
    if bits == 8:
        tag = lut_header(b'mft1')
        for _ in range(4):
            tag.extend(range(256))
        for c in range(2):
            for m in range(2):
                for y in range(2):
                    for k in range(2):
                        tag.extend((255 if k else 0, 128, 128))
        for _ in range(3):
            tag.extend(range(256))
    else:
        tag = lut_header(b'mft2')
        tag.extend(u16(2) + u16(2))  # 2-entry input and output tables
        for _ in range(4):
            tag.extend(u16(0) + u16(65535))
        for c in range(2):
            for m in range(2):
                for y in range(2):
                    for k in range(2):
                        # Legacy lut16 L*=100 is 0xff00; a/b=0x8080 is
                        # deliberately a slight tint (+0.5), not neutral.
                        tag.extend(u16(65280 if k else 0) + u16(32896) + u16(32896))
        for _ in range(3):
            tag.extend(u16(0) + u16(65535))
    return profile('CMYK', 'Lab ', [('wtpt', xyz(*D50)), ('A2B0', bytes(tag))])


def cmyk_xyz_lut(bits):
    # Deliberately map C/M/Y to linear Adobe RGB primaries, attenuated by K.
    # Green lies outside sRGB: converting through display RGB loses information.
    tag = lut_header(b'mft1' if bits == 8 else b'mft2')
    if bits == 16:
        tag.extend(u16(2) + u16(2))
    for _ in range(4):
        tag.extend(bytes(range(256)) if bits == 8 else u16(0) + u16(65535))
    primaries = ((.6097559, .3111242, .0194811),
                 (.2052401, .6256560, .0608902),
                 (.1492240, .0632197, .7448387))
    scale = 128 if bits == 8 else 32768
    for c in range(2):
        for m in range(2):
            for y in range(2):
                for k in range(2):
                    for axis in range(3):
                        value = round((1-k) * sum(v*p[axis] for v, p in zip((c,m,y), primaries)) * scale)
                        tag.extend(bytes((value,)) if bits == 8 else u16(value))
    for _ in range(3):
        tag.extend(bytes(range(256)) if bits == 8 else u16(0) + u16(65535))
    return profile('CMYK', 'XYZ ', [('wtpt', xyz(*D50)), ('A2B0', bytes(tag))])



def rgb_bidirectional_lut(bits, pcs='XYZ ', inverse=True):
    """Explicit RGB LUT pair with intentionally non-reciprocal B2A.

    XYZ: R/G are exchanged in PCS; the PCS is half-scale so B2A input
    tables expand XYZ u1.15 encoding. Its B2A also contracts red by 25%,
    proving that already-encoded same-space PDF paints must not roundtrip.
    Lab: R controls L*, G controls a*, B controls b* (legacy v2 Lab);
    its B2A contracts L* by 20% as another non-reciprocal inverse.
    """
    assert bits in (8, 16) and pcs in ('XYZ ', 'Lab ')
    kind = b'mft1' if bits == 8 else b'mft2'
    word = (lambda v: bytes((round(v * 255),))) if bits == 8 else (lambda v: u16(round(v * 65535)))
    def header():
        tag = bytearray(kind + b'\0' * 4 + bytes((3, 3, 2, 0)))
        tag.extend(b''.join(s15(1.0 if r == c else 0.0)
                             for r in range(3) for c in range(3)))
        if bits == 16:
            tag.extend(u16(5) + u16(2))
        return tag
    def tables(tag, backwards):
        for channel in range(3):
            for i in range(256 if bits == 8 else 5):
                t = i / (255 if bits == 8 else 4)
                # B2A XYZ PCS is encoded in u1.15: x=.5 corresponds to .25
                # at the table input; device-space values occupy 0..1.
                if backwards and pcs == 'XYZ ':
                    t = min(4*t, 1)
                elif backwards and pcs == 'Lab ' and channel == 0:
                    t = min(t * 65535/65280, 1)
                tag.extend(word(t))
    def clut(tag, backwards):
        for a in range(2):
            for b in range(2):
                for c in range(2):
                    if pcs == 'XYZ ':
                        values = (b, a, c)
                        if backwards:
                            values = (b * .75, a, c)
                        if not backwards:
                            # Device -> XYZ: 0.5 PCS is .25 of u1.15 range.
                            values = tuple(v * .25 for v in values)
                    else:
                        values = (a, b, c)
                        if backwards:
                            values = (a * .8, b, c)
                        if not backwards:
                            # ICC v2 Lab L*=100 is 0xff00 in a lut16 CLUT.
                            values = (a * 65280/65535, b, c)
                    for value in values:
                        tag.extend(word(value))
    def make(backwards):
        tag = header()
        tables(tag, backwards)
        clut(tag, backwards)
        for _ in range(3):
            tag.extend(bytes(range(256)) if bits == 8 else u16(0) + u16(65535))
        return bytes(tag)
    tags = [('wtpt', xyz(*D50)), ('A2B0', make(False))]
    if inverse:
        tags.append(('B2A0', make(True)))
    return profile('RGB ', pcs, tags)

def lcms_oracle(profiles, samples):
    libname = ctypes.util.find_library('lcms2')
    if not libname:
        return None
    lib = ctypes.CDLL(libname)
    lib.cmsOpenProfileFromMem.argtypes = (ctypes.c_void_p, ctypes.c_uint32)
    lib.cmsOpenProfileFromMem.restype = ctypes.c_void_p
    lib.cmsCreate_sRGBProfile.argtypes = ()
    lib.cmsCreate_sRGBProfile.restype = ctypes.c_void_p
    lib.cmsFormatterForColorspaceOfProfile.argtypes = (ctypes.c_void_p, ctypes.c_uint32, ctypes.c_uint32)
    lib.cmsFormatterForColorspaceOfProfile.restype = ctypes.c_uint32
    lib.cmsCreateTransform.argtypes = (ctypes.c_void_p, ctypes.c_uint32, ctypes.c_void_p,
                                      ctypes.c_uint32, ctypes.c_uint32, ctypes.c_uint32)
    lib.cmsCreateTransform.restype = ctypes.c_void_p
    lib.cmsDoTransform.argtypes = (ctypes.c_void_p, ctypes.c_void_p, ctypes.c_void_p, ctypes.c_uint32)
    lib.cmsDeleteTransform.argtypes = (ctypes.c_void_p,)
    lib.cmsCloseProfile.argtypes = (ctypes.c_void_p,)
    out_profile = lib.cmsCreate_sRGBProfile()
    if not out_profile:
        raise RuntimeError('LittleCMS could not create its independent sRGB destination')
    result = {}
    try:
        for name, content in profiles.items():
            raw = ctypes.create_string_buffer(content)
            src = lib.cmsOpenProfileFromMem(raw, len(content))
            if not src:
                raise RuntimeError(f'LittleCMS rejected fixture {name}')
            try:
                src_format = lib.cmsFormatterForColorspaceOfProfile(src, 1, 0)
                dst_format = lib.cmsFormatterForColorspaceOfProfile(out_profile, 1, 0)
                # Intent 1: relative colorimetric. No black-point compensation.
                transform = lib.cmsCreateTransform(src, src_format, out_profile, dst_format, 1, 0)
                if not transform:
                    raise RuntimeError(f'LittleCMS could not transform fixture {name}')
                try:
                    colors = []
                    for sample in samples[name]:
                        src_pixels = (ctypes.c_uint8 * len(sample))(*sample)
                        rgb = (ctypes.c_uint8 * 3)()
                        lib.cmsDoTransform(transform, src_pixels, rgb, 1)
                        colors.append(list(rgb))
                    result[name] = colors
                finally:
                    lib.cmsDeleteTransform(transform)
            finally:
                lib.cmsCloseProfile(src)
    finally:
        lib.cmsCloseProfile(out_profile)
    return result


def main(directory):
    directory = Path(directory)
    directory.mkdir(parents=True, exist_ok=True)
    profiles = {'gray-linear': gray(), 'rgb-swapped': swapped_rgb(),
                'cmyk-mft1': cmyk_lut(8), 'cmyk-mft2': cmyk_lut(16),
                'rgb-linear': linear_rgb(), 'rgb-adobe': adobe_rgb(),
                'rgb-lut8-xyz': rgb_bidirectional_lut(8),
                'rgb-lut16-lab': rgb_bidirectional_lut(16, 'Lab ')}
    samples = {'gray-linear': [[0], [128], [255], [51], [204]],
               'rgb-swapped': [[255, 0, 0], [0, 255, 0], [0, 0, 255]],
               'cmyk-mft1': [[0, 0, 0, 0], [0, 0, 0, 128], [0, 0, 0, 255]],
               'cmyk-mft2': [[0, 0, 0, 0], [0, 0, 0, 128], [0, 0, 0, 255]],
               'rgb-linear': [[128, 0, 127]],
               'rgb-adobe': [[64, 191, 0]],
               'rgb-lut8-xyz': [[128, 0, 128], [255, 0, 0]],
               'rgb-lut16-lab': [[191, 128, 128], [128, 128, 128]]}
    for name, content in profiles.items():
        (directory / (name + '.icc')).write_bytes(content)
    # Malformed examples: invalid header size/signature and truncated tag payload.
    (directory / 'gray-bad-signature.icc').write_bytes(
        profiles['gray-linear'][:36] + b'fail' + profiles['gray-linear'][40:])
    (directory / 'gray-truncated.icc').write_bytes(profiles['gray-linear'][:-4])
    report = {'samples': samples,
              'expected_srgb_lcms': lcms_oracle(profiles, samples),
              'pdf_dict_cases': {
                  'valid_gray': {'N': 1, 'ColorSpace': '[/ICCBased profile]', 'Decode': '[0 1]'},
                  'mismatch_rgb_N4_no_alternate': {'N': 4, 'profile': 'rgb-swapped.icc', 'expected': 'DeviceCMYK'},
                  'mismatch_rgb_N4_device_alternate': {'N': 4, 'profile': 'rgb-swapped.icc',
                                                      'Alternate': '/DeviceCMYK', 'expected': 'DeviceCMYK'},
                  'bad_signature_no_alternate': {'N': 1, 'profile': 'gray-bad-signature.icc', 'expected': 'DeviceGray'},
                  'bad_signature_with_alternate': {'N': 1, 'profile': 'gray-bad-signature.icc',
                                                   'Alternate': '/DeviceGray', 'expected': 'DeviceGray'}},
              'image_graphics_shading_checks': [
                  'Embed gray-linear.icc as /N 1 ICCBased image /Width 1 /Height 1 /BitsPerComponent 8, sample 128; compare to expected_srgb_lcms.gray-linear[1].',
                  'Set /ColorSpace << /C [/ICCBased gray-linear-profile] >>; /C cs 0.5019607843 sc; fill and stroke should match same oracle sample.',
                  'Type 2 axial shading in /C with FunctionType 2, C0 [0], C1 [1], N 1; midpoint should be around sRGB 188, not 128.',
                  'Embed RGB-swapped ICC with 1x1 /Device samples [255 0 0] and graphic /C cs 1 0 0 sc; both must match independent rgb-swapped[0].',
                  'Embed each CMYK mft1/mft2 with 1x1 sample [0 0 0 0], graphic /C cs 0 0 0 0 sc and 2-stop axial CMYK shading; compare endpoints to oracle.',
                  'rgb-linear[0] is the group-space red/half-blue composite for all four quadrants of synth-icc-page-linear-blend.',
                  'rgb-adobe[0] is the group-space green/quarter-red composite for all four quadrants of synth-icc-page-wide-gamut-blend; never convert and clip each source to sRGB before blending.',
                  'rgb-lut8-xyz[0] is the encoded-component page composite (half red, half blue), independent of an sRGB blend. Its explicit B2A contracts red by 25%, so same-profile paints must bypass it.',
                  'rgb-lut16-lab[0] is the L*75 Form composite including a same-profile nested Form. Its explicit B2A contracts L* by 20%; half-opacity over green acts after conversion to sRGB.'
              ]}
    (directory / 'oracle.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    if len(sys.argv) != 2:
        raise SystemExit('usage: icc-fixtures.py OUTPUT_DIRECTORY')
    main(sys.argv[1])
