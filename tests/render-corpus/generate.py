#!/usr/bin/env python3
"""Generate the hand-authored rendering corpus and its content-addressed manifest.

Only the Python standard library is required. Run from any working directory:
    python3 tests/render-corpus/generate.py
"""

import hashlib
import json
from pathlib import Path
import zlib
import icc_profiles

ROOT = Path(__file__).resolve().parent
OUT = ROOT / "generated"


class PDF:
    def __init__(self):
        self.objects = []

    def reserve(self):
        self.objects.append(None)
        return len(self.objects)

    def add(self, value):
        number = self.reserve()
        self.set(number, value)
        return number

    def set(self, number, value):
        self.objects[number - 1] = value.encode("ascii") if isinstance(value, str) else value

    def stream(self, dictionary, data):
        if isinstance(data, str):
            data = data.encode("ascii")
        return self.add(b"<< " + dictionary.encode("ascii") + b" /Length " + str(len(data)).encode("ascii") + b" >>\nstream\n" + data + b"\nendstream")

    def serialize(self, root):
        result = bytearray(b"%PDF-1.4\n%\xe2\xe3\xcf\xd3\n")
        offsets = [0]
        for number, obj in enumerate(self.objects, 1):
            if obj is None:
                raise ValueError(f"unassigned object {number}")
            offsets.append(len(result))
            result.extend(f"{number} 0 obj\n".encode("ascii"))
            result.extend(obj)
            result.extend(b"\nendobj\n")
        xref = len(result)
        result.extend(f"xref\n0 {len(offsets)}\n0000000000 65535 f \n".encode("ascii"))
        for offset in offsets[1:]:
            result.extend(f"{offset:010d} 00000 n \n".encode("ascii"))
        result.extend(f"trailer\n<< /Size {len(offsets)} /Root {root} 0 R >>\nstartxref\n{xref}\n%%EOF\n".encode("ascii"))
        return bytes(result)


def page(content, *, extra_resources="", page_options="", parent_options="", setup=None):
    pdf = PDF()
    catalog = pdf.reserve()
    pages = pdf.reserve()
    font = pdf.add("<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
    additions = setup(pdf) if setup else ""
    contents = pdf.stream("", content)
    resources = f"<< /Font << /F1 {font} 0 R >> {extra_resources} {additions} >>"
    media_box = "" if "/MediaBox" in page_options else "/MediaBox [0 0 320 240]"
    leaf = pdf.add(f"<< /Type /Page /Parent {pages} 0 R {media_box} {page_options} /Resources {resources} /Contents {contents} 0 R >>")
    pdf.set(pages, f"<< /Type /Pages /Kids [{leaf} 0 R] /Count 1 {parent_options} >>")
    pdf.set(catalog, f"<< /Type /Catalog /Pages {pages} 0 R >>")
    return pdf.serialize(catalog)


def image(pdf, width, height, bits, color, pixels, extra=""):
    data = zlib.compress(bytes(pixels), 9)
    return pdf.stream(f"/Type /XObject /Subtype /Image /Width {width} /Height {height} /BitsPerComponent {bits} /ColorSpace /{color} /Filter /FlateDecode {extra}", data)


def packed_images(pdf):
    # Width 5 forces each row of each image to have unused padding bits.
    one = image(pdf, 5, 2, 1, "DeviceGray", [0b10101000, 0b01010000], "/Decode [1 0]")
    two = image(pdf, 5, 2, 2, "DeviceGray", [0x1b, 0x00, 0xe4, 0xc0], "/Decode [0 1]")
    four = image(pdf, 5, 2, 4, "DeviceGray", [0x04, 0x8c, 0xf0, 0xfc, 0x84, 0x00], "/Decode [1 0]")
    return f"/XObject << /I1 {one} 0 R /I2 {two} 0 R /I4 {four} 0 R >>"


def stencil(pdf):
    # An image mask uses Decode to choose which of the packed bits paints.
    mask = pdf.stream("/Type /XObject /Subtype /Image /Width 8 /Height 8 /ImageMask true /BitsPerComponent 1 /Decode [0 1]", bytes([0x00, 0x7e, 0x42, 0x5a, 0x5a, 0x42, 0x7e, 0x00]))
    return f"/XObject << /Mask {mask} 0 R >>"


def soft_mask(pdf):
    alpha = image(pdf, 4, 1, 8, "DeviceGray", [0, 85, 170, 255])
    rgb = image(pdf, 4, 1, 8, "DeviceRGB", [230, 30, 30] * 4, f"/SMask {alpha} 0 R")
    return f"/XObject << /Im {rgb} 0 R >>"


def form(pdf):
    # The blue rectangle intentionally extends beyond the form's bounding box.
    obj = pdf.stream("/Type /XObject /Subtype /Form /FormType 1 /BBox [0 0 100 70] /Matrix [1 0.25 -0.25 1 0 0] /Resources << >>", "0.15 0.4 0.9 rg -30 -30 160 130 re f\n1 1 1 rg 10 10 80 50 re f\n0.9 0.25 0.1 rg 20 20 60 30 re f\n")
    return f"/XObject << /Fm {obj} 0 R >>"


def transparency(pdf):
    gs = pdf.add("<< /Type /ExtGState /ca 0.5 /CA 0.5 >>")
    return f"/ExtGState << /Half {gs} 0 R >>"


def shading(pdf):
    func = pdf.add("<< /FunctionType 2 /Domain [0 1] /C0 [1 0.15 0.1] /C1 [0.1 0.2 1] /N 1 >>")
    shade = pdf.add(f"<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [30 0 290 0] /Domain [0 1] /Function {func} 0 R /Extend [true true] >>")
    return f"/Shading << /Grad {shade} 0 R >>"


def fax_images(pdf):
    def packed(bits):
        padded = bits + "0" * (-len(bits) % 8)
        return bytes(int(padded[i:i + 8], 2) for i in range(0, len(padded), 8))

    # Two rows: eight black samples, then eight white.
    group3 = packed("00110101" + "000101" + "10011")
    group4 = packed("001" + "00110101" + "000101" + "0001")
    refs = []
    for name, k, data in (("Fax3", 0, group3), ("Fax4", -1, group4)):
        obj = pdf.stream(f"/Type /XObject /Subtype /Image /Width 8 /Height 2 /BitsPerComponent 1 /ColorSpace /DeviceGray /Filter /CCITTFaxDecode /DecodeParms << /K {k} /Columns 8 /Rows 2 /EndOfBlock false >>", data)
        refs.append(f"/{name} {obj} 0 R")
    return "/XObject << " + " ".join(refs) + " >>"


def icc_case(name):
    profile, sample, start, end = {
        "gray-linear": (icc_profiles.gray, [128], [0], [1]),
        "rgb-swapped": (icc_profiles.swapped_rgb, [255, 0, 0], [1, 0, 0], [0, 1, 0]),
        "cmyk-mft1": (lambda: icc_profiles.cmyk_lut(8), [0, 0, 0, 128], [0, 0, 0, 0], [0, 0, 0, 1]),
        "cmyk-mft2": (lambda: icc_profiles.cmyk_lut(16), [0, 0, 0, 128], [0, 0, 0, 0], [0, 0, 0, 1]),
    }[name]
    def setup(pdf):
        icc = pdf.stream(f"/N {len(sample)}", profile())
        image = pdf.stream("/Type /XObject /Subtype /Image /Width 1 /Height 1 "
                           f"/BitsPerComponent 8 /ColorSpace [/ICCBased {icc} 0 R]", bytes(sample))
        c0, c1 = " ".join(map(str, start)), " ".join(map(str, end))
        function = pdf.add(f"<< /FunctionType 2 /Domain [0 1] /C0 [{c0}] /C1 [{c1}] /N 1 >>")
        shade = pdf.add(f"<< /ShadingType 2 /ColorSpace [/ICCBased {icc} 0 R] "
                        f"/Coords [30 0 290 0] /Function {function} 0 R /Extend [true true] >>")
        return f"/ColorSpace << /Managed [/ICCBased {icc} 0 R] >> /XObject << /Im {image} 0 R >> /Shading << /Grad {shade} 0 R >>"
    values = " ".join(format(v / 255, ".12g") for v in sample)
    return page(f"q 80 0 0 60 20 150 cm /Im Do Q "
                f"/Managed cs {values} sc 120 150 80 60 re f "
                f"/Managed CS {values} SC 10 w 230 160 50 40 re S "
                "q 30 40 260 60 re W n /Grad sh Q "
                f"0 0 0 rg BT /F1 15 Tf 1 0 0 1 30 115 Tm ({name}: image / fill / stroke) Tj ET",
                setup=setup)


def icc_range():
    def setup(pdf):
        profile = pdf.stream("/N 1 /Range [0.2 0.8]", icc_profiles.gray())
        image_dict = f"/Type /XObject /Subtype /Image /Width 1 /Height 1 /BitsPerComponent 8 /ColorSpace [/ICCBased {profile} 0 R]"
        low = pdf.stream(image_dict, b"\0")
        high = pdf.stream(image_dict + " /Decode [0.8 0.2]", b"\0")
        return f"/ColorSpace << /Managed [/ICCBased {profile} 0 R] >> /XObject << /Low {low} 0 R /High {high} 0 R >>"
    return page("q 60 0 0 70 20 110 cm /Low Do Q q 60 0 0 70 95 110 cm /High Do Q "
                "/Managed cs 0.2 sc 170 110 60 70 re f 0.8 sc 245 110 60 70 re f "
                "0 0 0 rg BT /F1 12 Tf 1 0 0 1 20 80 Tm (default / inverted Decode / 0.2 / 0.8) Tj ET",
                setup=setup)


def vertical_cid(default_dw2=False):
    pdf = PDF()
    catalog, pages = pdf.reserve(), pdf.reserve()
    content = pdf.stream('', 'BT /F1 20 Tf 1 0 0 1 100 250 Tm <41814081418142> Tj ET')
    parent = pdf.stream('', '''/WMode 1 def
2 begincodespacerange
<00> <7f>
<8100> <81ff>
endcodespacerange
2 begincidrange
<41> <41> 2
<8140> <8142> 4
endcidrange''')
    encoding = pdf.stream(f'/UseCMap {parent} 0 R', '''1 begincidchar
<41> 3
endcidchar''')
    tounicode = pdf.stream('', '''2 begincodespacerange
<00> <7f>
<8100> <81ff>
endcodespacerange
4 beginbfchar
<41> <0041>
<8140> <0042>
<8141> <0043>
<8142> <0044>
endbfchar''')
    descriptor = pdf.add('<< /Type /FontDescriptor /FontName /Fixture /Flags 4 /FontBBox [0 -200 1000 900] /ItalicAngle 0 /Ascent 800 /Descent -200 /CapHeight 700 /StemV 80 >>')
    dw2 = '' if default_dw2 else '/DW2 [760 -1300]'
    cid = pdf.add(f'<< /Type /Font /Subtype /CIDFontType2 /BaseFont /Fixture /FontDescriptor {descriptor} 0 R /CIDSystemInfo << /Registry (Adobe) /Ordering (Identity) /Supplement 0 >> /DW 1000 /W [3 [800 1000 1200 600]] {dw2} /W2 [3 [-900 400 850 -1100 500 900] 5 5 -1200 600 950] /CIDToGIDMap /Identity >>')
    font = pdf.add(f'<< /Type /Font /Subtype /Type0 /BaseFont /Fixture /Encoding {encoding} 0 R /DescendantFonts [{cid} 0 R] /ToUnicode {tounicode} 0 R >>')
    leaf = pdf.add(f'<< /Type /Page /Parent {pages} 0 R /MediaBox [0 0 400 400] /Resources << /Font << /F1 {font} 0 R >> >> /Contents {content} 0 R >>')
    pdf.set(pages, f'<< /Type /Pages /Kids [{leaf} 0 R] /Count 1 >>')
    pdf.set(catalog, f'<< /Type /Catalog /Pages {pages} 0 R >>')
    return pdf.serialize(catalog)


BLEND_MODES = ["Normal", "Multiply", "Screen", "Overlay", "Darken", "Lighten",
               "ColorDodge", "ColorBurn", "HardLight", "SoftLight", "Difference",
               "Exclusion", "Hue", "Saturation", "Color", "Luminosity"]


def blend_modes(pdf):
    states = [pdf.add(f"<< /BM /{mode} /ca 0.65 >>") for mode in BLEND_MODES]
    return "/ExtGState << " + " ".join(f"/B{i} {obj} 0 R" for i, obj in enumerate(states)) + " >>"


def blend_grid():
    commands = []
    for i, mode in enumerate(BLEND_MODES):
        x, y = (i % 4) * 80, 180 - (i // 4) * 60
        commands.append(f"0.2 0.6 0.8 rg {x+5} {y+8} 60 38 re f "
                        f"q /B{i} gs 0.9 0.3 0.1 rg {x+25} {y+12} 45 28 re f Q "
                        f"0 0 0 rg BT /F1 7 Tf 1 0 0 1 {x+5} {y+49} Tm ({mode}) Tj ET")
    return "\n".join(commands)


def transparency_groups(pdf):
    half = pdf.add("<< /ca 0.5 /BM /Multiply >>")
    outer = pdf.add("<< /ca 0.75 >>")
    forms = []
    for i, (isolated, knockout) in enumerate([(True, False), (False, False), (True, True), (False, True)]):
        group = f"/S /Transparency /CS /DeviceRGB /I {str(isolated).lower()} /K {str(knockout).lower()}"
        form = pdf.stream(f"/Type /XObject /Subtype /Form /BBox [0 0 100 80] /Group << {group} >> "
                          f"/Resources << /ExtGState << /Half {half} 0 R >> >>",
                          "/Half gs 1 0 0 rg 0 0 65 80 re f 0 1 0 rg 35 0 65 80 re f")
        forms.append(f"/G{i} {form} 0 R")
    return f"/ExtGState << /Outer {outer} 0 R >> /XObject << {' '.join(forms)} >>"


def group_panels():
    commands = []
    for i, label in enumerate(["isolated", "non-isolated", "isolated knockout", "non-isolated knockout"]):
        x, y = 30 + (i % 2) * 160, 135 - (i // 2) * 110
        commands.append(f"0 0 1 rg {x} {y} 100 80 re f "
                        f"q 1 0 0 1 {x} {y} cm /Outer gs /G{i} Do Q "
                        f"0 0 0 rg BT /F1 9 Tf 1 0 0 1 {x} {y-13} Tm ({label}) Tj ET")
    return "\n".join(commands)


def graphics_soft_masks(pdf):
    half = pdf.add("<< /ca 0.5 >>")
    mask = pdf.stream("/Type /XObject /Subtype /Form /BBox [0 0 100 80] "
                      "/Group << /S /Transparency /CS /DeviceRGB /I true >> "
                      f"/Resources << /ExtGState << /Half {half} 0 R >> >>",
                      "/Half gs 1 0 0 rg 0 0 50 80 re f 1 1 1 rg 50 0 50 80 re f")
    alpha = pdf.add(f"<< /SMask << /S /Alpha /G {mask} 0 R >> >>")
    luminosity = pdf.add(f"<< /SMask << /S /Luminosity /G {mask} 0 R /BC [0 0 0] >> >>")
    return f"/ExtGState << /Alpha {alpha} 0 R /Luminosity {luminosity} 0 R >>"


def function_surface(pdf, sampled=False):
    dictionary = "/Domain [0 1 0 1] /Range [0 1 0 1 0 1]"
    if sampled:
        function = pdf.stream("/FunctionType 0 /Size [2 2] /BitsPerSample 8 " + dictionary,
                              bytes([0, 0, 0, 255, 0, 0, 0, 255, 0, 255, 255, 0]))
    else:
        function = pdf.stream("/FunctionType 4 " + dictionary, "{ 0 }")
    shading = pdf.add(f"<< /ShadingType 1 /ColorSpace /DeviceRGB /Domain [0 1 0 1] "
                      f"/Matrix [80 0 0 80 10 10] /BBox [10 10 90 90] /Function {function} 0 R >>")
    return f"/Shading << /Surface {shading} 0 R >>"


def cubic_sampled_shading(pdf, surface=False):
    if surface:
        samples = bytes(v for green in (0, 255) for red in (0, 255, 255, 0)
                        for v in (red, green, 0))
        function = pdf.stream(
            "/FunctionType 0 /Order 3 /Size [4 2] /BitsPerSample 8 "
            "/Domain [0 1 0 1] /Range [0 1 0 1 0 1] /Decode [0 0.8 0 1 0 1]", samples)
        shading = pdf.add(
            f"<< /ShadingType 1 /ColorSpace /DeviceRGB /Domain [0 1 0 1] "
            f"/Matrix [80 0 0 80 10 10] /BBox [10 10 90 90] /Function {function} 0 R >>")
    else:
        function = pdf.stream(
            "/FunctionType 0 /Order 3 /Size [4] /BitsPerSample 8 /Domain [0 1] "
            "/Range [0 1] /Decode [0 0.8]", bytes([0, 255, 255, 0]))
        shading = pdf.add(f"<< /ShadingType 2 /ColorSpace /DeviceGray /Coords [10 0 90 0] "
                          f"/Function {function} 0 R /Extend [false false] >>")
    return f"/Shading << /Cubic {shading} 0 R >>"


def calculator_gradient(pdf):
    function = pdf.stream("/FunctionType 4 /Domain [0 1] /Range [0 1 0 1 0 1]",
                          "{ dup .5 lt { 2 mul 0 0 } { 2 mul 1 sub 1 exch sub 0 exch 1 } ifelse }")
    shading = pdf.add(f"<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [30 0 290 0] "
                      f"/Function {function} 0 R /Extend [false false] >>")
    return f"/Shading << /Grad {shading} 0 R >>"


def thin_stroke_panels():
    commands = []
    for x, width, label in [(30, "0.3", "Subpixel"), (180, "0", "Hairline")]:
        commands.append(f"q .35 0 0 .35 {x} 65 cm 0 G {width} w")
        commands.extend(f"0 {y} m 300 {y} l S" for y in range(0, 301, 10))
        commands.append(f"Q BT /F1 14 Tf 1 0 0 1 {x} 35 Tm ({label}) Tj ET")
    return "\n".join(commands)


def page_blend_form(pdf):
    form = pdf.stream(
        "/Type /XObject /Subtype /Form /BBox [0 0 100 50] /Resources <<>> "
        "/Group << /S /Transparency /CS /DeviceRGB /I false >>",
        "0 0 1 rg 0 0 100 50 re f")
    return f"/ExtGState << /D << /BM /Difference >> >> /XObject << /Fm {form} 0 R >>"


def icc_page_blending(wide=False):
    def setup(pdf):
        profile = pdf.stream("/N 3", icc_profiles.adobe_rgb() if wide else icc_profiles.linear_rgb())
        space = f"[/ICCBased {profile} 0 R]"
        colors = f"/ColorSpace << /Working {space} >>"
        source = "1 0 0" if wide else "0 0 1"
        pixels = b"\xff\0\0" if wide else b"\0\0\xff"
        im = pdf.stream(f"/Type /XObject /Subtype /Image /Width 1 /Height 1 "
                        f"/BitsPerComponent 8 /ColorSpace {space}", pixels)
        fn = pdf.add(f"<< /FunctionType 2 /Domain [0 1] /C0 [{source}] /C1 [{source}] /N 1 >>")
        shade = pdf.add(f"<< /ShadingType 2 /ColorSpace {space} /Coords [0 0 200 0] "
                        f"/Function {fn} 0 R /Extend [true true] >>")
        pattern = pdf.stream(
            "/Type /Pattern /PatternType 1 /PaintType 1 /TilingType 1 /BBox [0 0 10 10] "
            f"/XStep 10 /YStep 10 /Resources << {colors} >>",
            f"/Working cs {source} sc 0 0 10 10 re f")
        return (f"{colors} /XObject << /Im {im} 0 R >> /Shading << /Shade {shade} 0 R >> "
                f"/Pattern << /Tile {pattern} 0 R >> "
                f"/ExtGState << /Alpha << /ca {'0.25' if wide else '0.5'} >> >>")
    backdrop = "0 1 0" if wide else "1 0 0"
    source = "1 0 0" if wide else "0 0 1"
    return page(
        f"/Working cs {backdrop} sc 0 0 200 200 re f /Alpha gs "
        f"{source} sc 0 100 100 100 re f "
        "q 100 0 0 100 100 100 cm /Im Do Q "
        "q 0 0 100 100 re W n /Shade sh Q "
        "/Pattern cs /Tile scn 100 0 100 100 re f",
        setup=setup, page_options="/MediaBox [0 0 200 200] "
        "/Group << /S /Transparency /CS /Working /I true >>")


def icc_form_blending():
    def setup(pdf):
        profile = pdf.stream("/N 3", icc_profiles.linear_rgb())
        resources = f"/ColorSpace << /Working [/ICCBased {profile} 0 R] >> " \
                    "/ExtGState << /Blue << /ca 0.5 >> >>"
        forms = []
        for name, isolated in [("Isolated", True), ("NonIsolated", False)]:
            commands = "/Working cs "
            if isolated:
                commands += "1 0 0 sc 0 0 100 100 re f "
            commands += "/Blue gs 0 0 1 sc 0 0 100 100 re f"
            form = pdf.stream(
                f"/Type /XObject /Subtype /Form /BBox [0 0 100 100] /Resources << {resources} >> "
                f"/Group << /S /Transparency {'/CS /Working' if isolated else ''} /I {str(isolated).lower()} >>", commands)
            if not isolated:
                # Nonisolated groups inherit their parent's blending space.
                form = pdf.stream(
                    "/Type /XObject /Subtype /Form /BBox [0 0 100 100] "
                    f"/Resources << {resources} /XObject << /Inner {form} 0 R >> >> "
                    "/Group << /S /Transparency /CS /Working /I true >>",
                    "/Working cs 1 0 0 sc 0 0 100 100 re f /Inner Do")
            forms.append(f"/{name} {form} 0 R")
        return (f"/XObject << {' '.join(forms)} >> /ExtGState << "
                "/Half << /ca 0.5 >> /HalfMultiply << /ca 0.5 /BM /Multiply >> >>")
    return page(
        "0 1 0 rg 0 0 200 100 re f "
        "q 1 0 0 1 0 100 cm /Isolated Do Q "
        "q 1 0 0 1 100 100 cm /NonIsolated Do Q "
        "q /Half gs /Isolated Do Q "
        "q 1 0 0 1 100 0 cm /HalfMultiply gs /Isolated Do Q",
        setup=setup, page_options="/MediaBox [0 0 200 200]")


def icc_inherited_nested_mask():
    def setup(pdf):
        wide = pdf.stream("/N 3", icc_profiles.adobe_rgb())
        linear = pdf.stream("/N 3", icc_profiles.linear_rgb())
        colors = f"/ColorSpace << /Wide [/ICCBased {wide} 0 R] /Linear [/ICCBased {linear} 0 R] >>"
        states = "/ExtGState << /Quarter << /ca 0.25 >> /Half << /ca 0.5 >> >>"
        forms = []
        for name, initial in [("Inherited", ""), ("Direct", "/Wide cs 0 1 0 sc ")]:
            form = pdf.stream(
                f"/Type /XObject /Subtype /Form /BBox [0 0 100 100] "
                f"/Resources << {colors} {states} >> "
                "/Group << /S /Transparency /CS /Wide /I true >>",
                initial + "0 0 100 100 re f /Quarter gs /Wide cs 1 0 0 sc 0 0 100 100 re f")
            forms.append(f"/{name} {form} 0 R")
        inner = pdf.stream(
            f"/Type /XObject /Subtype /Form /BBox [0 0 100 100] "
            f"/Resources << {colors} {states} >> "
            "/Group << /S /Transparency /I false >>",
            "/Linear cs /Half gs 0 0 1 sc 0 0 100 100 re f")
        outer = pdf.stream(
            f"/Type /XObject /Subtype /Form /BBox [0 0 100 100] "
            f"/Resources << {colors} /XObject << /Inner {inner} 0 R >> >> "
            "/Group << /S /Transparency /CS /Linear /I true >>",
            "/Linear cs 1 0 0 sc 0 0 100 100 re f /Inner Do")
        mask = pdf.stream(
            "/Type /XObject /Subtype /Form /BBox [0 0 100 100] "
            "/Resources << /ExtGState << /Half << /ca 0.5 >> >> >> "
            "/Group << /S /Transparency /CS /DeviceRGB /I true >>",
            "/Half gs 1 1 1 rg 0 0 100 100 re f")
        return (f"{colors} /XObject << {' '.join(forms)} /Nested {outer} 0 R >> "
                f"/ExtGState << /MaskedHalf << /ca 0.5 /SMask << /S /Alpha /G {mask} 0 R >> >> >>")
    return page(
        "/Wide cs 0 1 0 sc "
        "q 1 0 0 1 0 100 cm /Inherited Do Q "
        "q 1 0 0 1 100 100 cm /Direct Do Q "
        "q /Nested Do Q "
        "q 1 0 0 1 100 0 cm 0 1 0 rg 0 0 100 100 re f /MaskedHalf gs /Nested Do Q",
        setup=setup, page_options="/MediaBox [0 0 200 200]")


def icc_luminosity_masks(boundary=False):
    def setup(pdf):
        profile = pdf.stream("/N 3", icc_profiles.adobe_rgb())
        colors = f"/ColorSpace << /Working [/ICCBased {profile} 0 R] >>"
        transfer = pdf.add("<< /FunctionType 2 /Domain [0 1] /C0 [1] /C1 [0] /N 1 >>")
        if boundary:
            cases = [(isolated, "[1 0 0]", "/Multiply gs 0 0 1 sc", invert)
                     for invert in (False, True) for isolated in (True, False)]
        else:
            cases = [(True, None, "0.5 0.5 0.5 sc", False),
                     (True, "[1 1 1]", "/Half gs 0 0 0 sc", False),
                     (False, "[1 1 1]", "/Half gs 0 0 0 sc", False),
                     (True, "[1 1 1]", "/Half gs 0 0 0 sc", True)]
        states = []
        bbox = "[20 20 80 80]" if boundary else "[0 0 100 100]"
        for i, (isolated, backdrop, paint, invert) in enumerate(cases):
            mask = pdf.stream(
                f"/Type /XObject /Subtype /Form /BBox {bbox} "
                f"/Resources << {colors} /ExtGState << /Half << /ca 0.5 >> "
                "/Multiply << /BM /Multiply >> >> >> "
                f"/Group << /S /Transparency /CS /Working /I {str(isolated).lower()} >>",
                f"/Working cs {paint} 0 0 100 100 re f")
            bc = f"/BC {backdrop}" if backdrop else ""
            tr = f"/TR {transfer} 0 R" if invert else ""
            states.append(f"/M{i} << /SMask << /S /Luminosity /G {mask} 0 R {bc} {tr} >> >>")
        return "/ExtGState << " + " ".join(states) + " >>"
    commands = []
    for i in range(4):
        x, y = (i % 2) * 100, 100 - (i // 2) * 100
        commands.append(f"q 1 0 0 1 {x} {y} cm /M{i} gs 1 0 0 rg 0 0 100 100 re f Q")
    return page(" ".join(commands), setup=setup, page_options="/MediaBox [0 0 200 200]")


def many_pages():
    pdf = PDF()
    catalog, pages = pdf.reserve(), pdf.reserve()
    font = pdf.add("<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
    leaves = []
    for number in range(1, 501):
        color = "0.1 0.3 0.9" if number % 2 else "0.9 0.2 0.1"
        content = pdf.stream("", f"{color} rg 20 20 280 150 re f "
                             f"0 0 0 rg BT /F1 24 Tf 1 0 0 1 20 195 Tm (Page {number:04d}) Tj ET")
        leaves.append(pdf.add(f"<< /Type /Page /Parent {pages} 0 R /MediaBox [0 0 320 240] "
                              f"/Resources << /Font << /F1 {font} 0 R >> >> /Contents {content} 0 R >>"))
    kids = " ".join(f"{leaf} 0 R" for leaf in leaves)
    pdf.set(pages, f"<< /Type /Pages /Kids [{kids}] /Count {len(leaves)} >>")
    pdf.set(catalog, f"<< /Type /Catalog /Pages {pages} 0 R >>")
    return pdf.serialize(catalog)


# Each entry contains PDF operators and an explicit human-readable visual oracle.
CASES = [
    dict(id="paths-basic", tags=["paths", "fill", "stroke"], description="Basic filled and stroked path primitives.", expected="A red rectangle, a blue triangle, and a thick green cubic curve on white.", content="1 0.2 0.15 rg 25 140 70 65 re f\n0.1 0.3 0.9 rg 120 140 m 190 140 l 155 210 l h f\n0 0.6 0.2 RG 7 w 220 145 m 250 220 280 80 305 190 c S\n"),
    dict(id="path-ctm", tags=["paths", "ctm"], description="A translated path with the CTM set before path construction.", expected="A red trapezoid has bottom vertices (30,50), (110,50) and top vertices (210,180), (30,180); the translation applies to the entire path.", content="q 1 0 0 1 100 0 cm 1 0.2 0.1 rg -70 50 m 10 50 l 110 180 l -70 180 l h f Q\n"),
    dict(id="clip-restore", tags=["clip", "graphics-state"], description="Clipping set inside q/Q and restored afterward.", expected="A red diamond on the left and a separate blue rectangle to its right; the blue rectangle remains entirely visible after Q.", content="q 75 40 m 125 110 l 75 180 l 25 110 l h W n 1 0.15 0.1 rg 20 35 110 150 re f Q\n0.1 0.3 0.9 rg 175 60 110 100 re f\n"),
    dict(id="clip-nested-evenodd", tags=["clip", "evenodd", "graphics-state"], description="Nested clips with an even-odd hole and restoration of the inner clip.", expected="A red left half of a rectangular ring and a blue right half; the center hole and everything outside the outer rectangle remain white.", content="q 25 35 270 170 re 100 80 120 80 re W* n\nq 25 35 135 170 re W n 1 0.15 0.1 rg 0 0 320 240 re f Q\n0.1 0.3 0.9 rg 160 0 160 240 re f Q\n"),
    dict(id="closepath-v", tags=["paths", "bezier", "closepath"], description="The v cubic shorthand immediately follows closepath.", expected="A dark triangular outline with a curved lobe rising from its starting point; the curve begins at the point restored by h.", content="0.1 0.1 0.1 RG 4 w 50 70 m 130 70 l 90 140 l h 30 190 170 190 v S\n"),
    dict(id="text-paint-order", tags=["text", "paint-order", "clip"], description="Text alternates with opaque painting, including path clipping.", expected="The red bar hides the middle of the large black BACK text; blue FRONT text is drawn above it. Green CLIPPED text is visible only inside the narrow rectangular clip.", content="BT /F1 52 Tf 1 0 0 1 25 150 Tm (BACK) Tj ET\n1 0.15 0.1 rg 20 162 215 22 re f\n0.1 0.2 0.9 rg BT /F1 30 Tf 1 0 0 1 105 155 Tm (FRONT) Tj ET\nq 60 45 140 55 re W n 0 0.6 0.15 rg BT /F1 42 Tf 1 0 0 1 25 57 Tm (CLIPPED) Tj ET Q\n"),
    dict(id="media-offset", tags=["page-box", "mediabox"], description="A page whose MediaBox has a nonzero lower-left corner.", expected="A 240 by 160 page with a red rectangle inset 20 points from its lower-left corner and a blue rectangle inset 20 points from its upper-right corner.", page_options="/MediaBox [100 200 340 360]", content="1 0.15 0.1 rg 120 220 65 45 re f 0.1 0.3 0.9 rg 255 295 65 45 re f\n"),
    dict(id="crop-offset", tags=["page-box", "cropbox"], description="CropBox selects an offset subrectangle of a larger MediaBox.", expected="The visible page is 200 by 140 points: a green rectangle sits inside it, while a red rectangle outside the CropBox is not visible.", page_options="/CropBox [60 40 260 180]", content="1 0 0 rg 5 190 40 35 re f 0 0.65 0.2 rg 80 60 160 100 re f\n"),
    dict(id="rotate-inherited", tags=["page-box", "rotate", "inheritance"], description="Page rotation inherited from its Pages parent.", expected="A landscape page is displayed as portrait, rotated clockwise 90 degrees; the red lower-left and blue upper-right markers move with the page.", parent_options="/Rotate 90", content="1 0.15 0.1 rg 20 20 75 50 re f 0.1 0.3 0.9 rg 225 170 75 50 re f\nBT /F1 24 Tf 1 0 0 1 100 110 Tm (ROTATE) Tj ET\n"),
    dict(id="text-transform", tags=["text", "matrix", "rise", "scaling", "spacing"], description="Text matrix rotation, text rise, horizontal scaling and character/word spacing.", expected="A diagonal ROTATED label, a BASE label with a raised UP, and a wide-spaced, horizontally compressed A B C line.", content="0 0 0 rg BT /F1 24 Tf 0.866 0.5 -0.5 0.866 40 145 Tm (ROTATED) Tj ET\nBT /F1 24 Tf 1 0 0 1 30 100 Tm (BASE) Tj 12 Ts (UP) Tj 0 Ts ET\nBT /F1 25 Tf 60 Tz 5 Tc 12 Tw 1 0 0 1 30 45 Tm (A B C) Tj ET\n"),
    dict(id="text-render-modes", tags=["text", "render-mode", "clip"], description="Fill, stroke, fill-and-stroke, invisible, and text-as-clip rendering modes.", expected="Top row shows black filled FILL, outlined STROKE and red-filled blue-outlined BOTH. INVISIBLE is absent; the bottom shows a green rectangle only inside large CLIP glyphs.", content="BT /F1 24 Tf 1 0 0 1 15 190 Tm 0 Tr (FILL) Tj 1 0 0 1 95 190 Tm 1 Tr (STROKE) Tj 1 0 0 1 205 190 Tm 1 0 0 rg 0 0 1 RG 2 Tr (BOTH) Tj ET\nBT /F1 28 Tf 1 0 0 1 15 135 Tm 3 Tr (INVISIBLE) Tj ET\nq BT /F1 72 Tf 1 0 0 1 25 40 Tm 7 Tr (CLIP) Tj ET 0 0.7 0.2 rg 0 0 320 130 re f Q\n"),
    dict(id="form-matrix-bbox", tags=["form", "xobject", "matrix", "clip"], description="A Form XObject with its own shear matrix and bounding-box clip, drawn twice.", expected="Two sheared blue frames enclosing white and red rectangles; the oversized blue form content is clipped to each form's transformed bounding box.", setup=form, content="q 1 0 0 1 50 65 cm /Fm Do Q\nq 0.8 0 0 0.8 205 95 cm /Fm Do Q\n"),
    dict(id="images-packed", tags=["image", "packed", "decode"], description="One-, two-, and four-bit grayscale images with padded rows and Decode arrays.", expected="Three enlarged two-row, five-column grayscale grids. The 1-bit grid alternates black and white; the 2-bit grid has four levels; the 4-bit grid has five levels reversed by Decode.", setup=packed_images, content="q 110 0 0 44 20 165 cm /I1 Do Q\nq 110 0 0 44 20 95 cm /I2 Do Q\nq 110 0 0 44 20 25 cm /I4 Do Q\nBT /F1 18 Tf 1 0 0 1 155 180 Tm (1 bit inverted) Tj 1 0 0 1 155 110 Tm (2 bit) Tj 1 0 0 1 155 40 Tm (4 bit inverted) Tj ET\n"),
    dict(id="image-stencil", tags=["image", "stencil", "mask"], description="A one-bit image mask painted with the current fill color.", expected="A large red pixelated square-ring pattern over a pale blue background, with the background visible through unpainted mask pixels.", setup=stencil, content="0.8 0.9 1 rg 30 25 200 190 re f\n1 0.1 0.1 rg q 160 0 0 160 50 40 cm /Mask Do Q\n"),
    dict(id="image-soft-mask", tags=["image", "soft-mask", "transparency"], description="An RGB image with a grayscale soft mask of four alpha levels.", expected="Four red image columns over a blue rectangle transition from fully transparent blue through two blended purples to opaque red.", setup=soft_mask, content="0.1 0.2 0.9 rg 30 55 260 130 re f\nq 240 0 0 110 40 65 cm /Im Do Q\n"),
    dict(id="alpha-overlap", tags=["transparency", "graphics-state"], description="Nonstroking alpha applied to overlapping opaque-colored rectangles.", expected="A half-transparent red rectangle and a half-transparent blue rectangle overlap on white; their overlap is darker and purple, with blue composited on top.", setup=transparency, content="/Half gs 1 0 0 rg 45 45 150 140 re f 0 0 1 rg 125 85 150 120 re f\n"),
    dict(id="axial-gradient", tags=["shading", "gradient"], description="An axial shading with a type 2 interpolation function.", expected="A broad horizontal gradient inside a rectangle changes smoothly from red on the left to blue on the right, with white outside.", setup=shading, content="q 30 45 260 150 re W n /Grad sh Q\n"),
    dict(id="images-fax", tags=["image", "ccitt", "group3", "group4"], description="CCITT one-dimensional Group 3 and two-dimensional Group 4 image streams.", expected="Two bordered panels have black top halves and white bottom halves, labelled G3 and G4.", setup=fax_images, content="q 110 0 0 100 30 70 cm /Fax3 Do Q q 110 0 0 100 180 70 cm /Fax4 Do Q 0 0 0 RG 30 70 110 100 re S 180 70 110 100 re S BT /F1 24 Tf 1 0 0 1 65 30 Tm (G3) Tj 1 0 0 1 215 30 Tm (G4) Tj ET\n"),
    dict(id="blend-modes", tags=["transparency", "blend-mode", "graphics-state"], description="All sixteen PDF blend modes with partial source alpha.", expected="A labelled 4x4 grid blends orange over cyan using each named mode; the overlap differs by mode and q/Q prevents state leaking into the next panel.", setup=blend_modes, content=blend_grid()),
    dict(id="transparency-groups", tags=["transparency", "group", "isolation", "knockout"], description="Isolated/non-isolated groups with and without knockout over an opaque blue backdrop.", expected="Four labelled panels apply group opacity once. Isolated groups retain red/green; non-isolated Multiply uses the blue backdrop. Knockout removes the first member under the second member's shape.", setup=transparency_groups, content=group_panels()),
    dict(id="graphics-soft-masks", tags=["transparency", "soft-mask", "luminosity", "graphics-state"], description="ExtGState alpha and luminosity masks from the same colored transparent form.", expected="The Alpha panel has uniform half-opacity orange over blue. Luminosity is weaker on the red-mask left half than on the white-mask right half. Blue outside each masked rectangle remains unchanged.", setup=graphics_soft_masks, content="0 0 1 rg 20 60 120 120 re f 180 60 120 120 re f q 1 0 0 1 30 80 cm /Alpha gs 1 0.2 0.1 rg 0 0 100 80 re f Q q 1 0 0 1 190 80 cm /Luminosity gs 1 0.2 0.1 rg 0 0 100 80 re f Q 0 0 0 rg BT /F1 15 Tf 1 0 0 1 55 30 Tm (Alpha) Tj 1 0 0 1 200 30 Tm (Luminosity) Tj ET"),
    dict(id="vertical-cid", tags=["text", "cid", "vertical", "cmap"], description="Inherited mixed-width CMap with explicit W2 and DW2 metrics.", expected="A, B, C and D descend vertically, with unequal advances and horizontal offsets from W2; the child CMap overrides its parent CID mapping.", factory=vertical_cid),
    dict(id="vertical-cid-default", tags=["text", "cid", "vertical", "cmap"], description="Vertical CID text with implicit DW2 metrics for the final glyph.", expected="The first three glyphs match vertical-cid; D uses the default vertical origin and 1000-unit downward advance.", factory=lambda: vertical_cid(True)),
    dict(id="icc-gray-linear", tags=["icc", "image", "shading", "color"], description="Linear Gray ICC conversion shared by image, fill, stroke and shading.", expected="The three top paints are the same light gray (near RGB 188), not device gray 128; the lower gradient has a light, not dark, midpoint.", factory=lambda: icc_case("gray-linear")),
    dict(id="icc-rgb-swapped", tags=["icc", "image", "shading", "color"], description="RGB ICC matrix/TRC profile with red and green primaries exchanged.", expected="All three top paints are green despite red source samples; the gradient runs green to red.", factory=lambda: icc_case("rgb-swapped")),
    dict(id="icc-cmyk-mft1", tags=["icc", "image", "shading", "lut"], description="CMYK ICC lut8 profile mapping increasing K from Lab black to white.", expected="The three top paints are mid-gray near RGB 119; the gradient runs black to white, opposite naive CMYK K.", factory=lambda: icc_case("cmyk-mft1")),
    dict(id="icc-cmyk-mft2", tags=["icc", "image", "shading", "lut"], description="CMYK ICC legacy lut16 Lab encoding with slightly tinted neutral components.", expected="The top paints agree near mid-gray with a small warm tint; the gradient runs black to near-white using legacy 0xff00 L encoding.", factory=lambda: icc_case("cmyk-mft2")),
    dict(id="icc-range-decode", tags=["icc", "image", "decode", "range"], description="ICCBased Range bounds values without renormalizing them; explicit Decode reverses image samples.", expected="The default image and 0.2 vector paint use the lower valid component; reversed Decode and the 0.8 vector paint match lighter gray. Poppler may ignore Range for default image Decode; the PDF default is the declared Range.", factory=icc_range),
    dict(id="function-sampled-2d", tags=["shading", "function", "sampled", "multidimensional"], description="Type 1 shading driven by a two-input sampled RGB function.", expected="An 80 by 80 square inset 10 points transitions from black at lower-left to red at lower-right, green at upper-left and yellow at upper-right; the surrounding border is white.", page_options="/MediaBox [0 0 100 100]", setup=lambda pdf: function_surface(pdf, True), content="/Surface sh"),
    dict(id="function-calculator-2d", tags=["shading", "function", "calculator", "matrix"], description="The same two-dimensional shading evaluated by a calculator function returning x, y and zero.", expected="The square and white border match function-sampled-2d, including all interior colors.", page_options="/MediaBox [0 0 100 100]", setup=function_surface, content="/Surface sh"),
    dict(id="function-calculator-branches", tags=["shading", "function", "calculator", "conditional"], description="A calculator axial gradient chooses different RGB output expressions on either side of its midpoint.", expected="Inside the clipped panel the left half runs black to red; the right half starts cyan and runs to blue, with a sharp red-to-cyan boundary at x=160. Outside the panel is white.", setup=calculator_gradient, content="q 30 45 260 150 re W n /Grad sh Q"),
    dict(id="many-pages", tags=["multipage", "performance", "text", "worker"], description="Five hundred individually numbered pages for scrolling, zoom and worker lifecycle checks.", expected="Each 320 by 240 page has its own Page NNNN label and a large blue rectangle on odd pages or red rectangle on even pages. Page 0500 is red; navigating back to Page 0001 restores blue.", factory=many_pages),
    dict(id="page-blend-backdrop", tags=["transparency", "page-group", "blend-mode", "form"], description="Direct painting above and a nested non-isolated Form below use Difference over painted and initially transparent parts of an isolated page.", expected="The entire left half is magenta (blue Difference red); the entire right half is blue, not yellow from the external white background. The direct upper half and nested Form lower half must match.", page_options="/MediaBox [0 0 100 100]", setup=page_blend_form, content="1 0 0 rg 0 0 50 100 re f q /D gs 0 0 1 rg 0 50 100 50 re f Q q /D gs /Fm Do Q"),
    dict(id="thin-strokes", tags=["stroke", "ctm", "hairline", "subpixel"], description="Positive 0.3-unit strokes and zero-width hairlines under a 0.35 scale transform.", expected="Two labelled panels of horizontal black lines. Subpixel lines on the left have much less coverage than the one-device-pixel hairlines on the right; positive widths must not be promoted to hairlines.", content=thin_stroke_panels()),
    dict(id="icc-page-linear-blend", tags=["icc", "transparency", "page-group", "image", "shading", "pattern"], description="Linear RGB page blending shared by vector, image, shading and tiling-pattern paints.", expected="All four quadrants are the same bright purple near RGB (188,0,188), not sRGB-space (128,0,128). Red and half-alpha blue are blended before display conversion.", factory=icc_page_blending),
    dict(id="icc-page-wide-gamut-blend", tags=["icc", "transparency", "page-group", "image", "shading", "pattern", "gamut"], description="Adobe RGB page blends green with quarter-alpha red without clipping source paints to sRGB first.", expected="All four quadrants agree near RGB (0,193,0), with no red contamination. The independent oracle converts the final Adobe RGB components (0.25,0.75,0), not separately clipped source colors.", factory=lambda: icc_page_blending(True)),
    dict(id="icc-form-blend-boundary", tags=["icc", "transparency", "form", "isolation", "blend-mode", "opacity"], description="Linear RGB isolated and non-isolated Forms return their source contribution to an sRGB parent before boundary opacity and Multiply.", expected="Top quadrants both show bright purple near (188,0,188). Bottom-left is near (94,128,94): half that display-space purple over green. Bottom-right is near (0,128,0): half-opacity Multiply over green. Untouched parent pixels must not make a profile roundtrip.", factory=icc_form_blending),
    dict(id="function-cubic-1d", tags=["shading", "function", "sampled", "cubic"], description="Order 3 four-knot grayscale interpolation with Decode headroom for cubic overshoot.", expected="A grayscale hill from black through the 0.8-valued inner knots, peaking near 0.9 (RGB 230) at the center. Linear interpolation incorrectly gives a flat 0.8 plateau. Uses uniform Catmull-Rom with secant endpoint tangents.", page_options="/MediaBox [0 0 100 100]", setup=cubic_sampled_shading, content="q 10 10 80 80 re W n /Cubic sh Q"),
    dict(id="function-cubic-2d", tags=["shading", "function", "sampled", "cubic", "multidimensional"], description="Tensor-product sampled function with cubic four-knot x and linear two-knot y.", expected="Inside an inset square, red rises from zero at both sides to about 230 in the middle, while green rises linearly from bottom to top. The center is near RGB (230,128,0). Samples advance along x first.", page_options="/MediaBox [0 0 100 100]", setup=lambda pdf: cubic_sampled_shading(pdf, True), content="/Cubic sh"),
    dict(id="icc-inherited-nested-mask", tags=["icc", "transparency", "form", "inheritance", "soft-mask", "gamut"], description="Wide-gamut inherited fill provenance, nested nonisolated linear group and boundary alpha mask plus opacity.", expected="Top quadrants match near (0,192,0): inherited Adobe green must not be recovered from clipped sRGB. Bottom-left nested linear groups produce (188,0,188). Bottom-right applies half mask and half opacity exactly once over green, near (47,191,47).", factory=icc_inherited_nested_mask),
    dict(id="icc-luminosity-components", tags=["icc", "transparency", "soft-mask", "luminosity", "transfer"], description="Adobe RGB luminosity masks composite encoded components before conversion to XYZ Y.", expected="Top-left opaque half-gray, top-right isolated half-black over white BC, and bottom-left nonisolated half-black over white BC all yield pale red near (255,200,200), not (255,128,128). Bottom-right inverts the luminosity through TR and is near (255,55,55).", factory=icc_luminosity_masks),
    dict(id="icc-luminosity-backdrop", tags=["icc", "transparency", "soft-mask", "luminosity", "bbox", "isolation", "blend-mode"], description="Isolated/nonisolated Multiply inside a bounded ICC luminosity mask over red BC, with and without inverse TR.", expected="Every quadrant has a 60-point inner square. Top-left center is near (255,239,239), top-right center white; both outside rings are near (255,176,176). Bottom-left center is near (255,16,16), bottom-right center red; both rings are near (255,79,79). BC and TR apply outside BBox too.", factory=lambda: icc_luminosity_masks(True)),
]


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    entries = []
    for case in CASES:
        options = {key: case[key] for key in ("setup", "page_options", "parent_options") if key in case}
        data = case["factory"]() if "factory" in case else page(case["content"], **options)
        case_id = "synth-" + case["id"]
        name = case_id + ".pdf"
        (OUT / name).write_bytes(data)
        entries.append({"id": case_id, "path": "generated/" + name,
                        "sha256": hashlib.sha256(data).hexdigest(), "bytes": len(data),
                        "tags": case["tags"], "description": case["description"],
                        "expected": case["expected"]})
    (ROOT / "synthetic.json").write_text(json.dumps({"schemaVersion": 1, "files": entries}, indent=2) + "\n", encoding="utf-8")
    print(f"Generated {len(entries)} PDFs and {ROOT / 'synthetic.json'}")


if __name__ == "__main__":
    main()
