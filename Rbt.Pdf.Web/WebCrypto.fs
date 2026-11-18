/// Encryption.Crypto on WebCrypto (crypto.subtle), for opening encrypted PDFs
/// in the browser. WebCrypto has no MD5 (needed by revisions 2-4), so MD5 is
/// implemented here; its AES-CBC always pads, so the raw (unpadded) operations
/// are built on the padded ones.
module Rbt.Pdf.Web.WebCrypto

open Fable.Core
open Fable.Core.JsInterop
open Rbt.Pdf

[<Emit("globalThis.crypto.subtle.digest($0, Uint8Array.from($1)).then(function(b){ return new Uint8Array(b); })")>]
let private digest (algorithm: string) (data: byte[]) : JS.Promise<byte[]> = jsNative

[<Emit("globalThis.crypto.subtle.importKey('raw', Uint8Array.from($0), {name:'AES-CBC'}, false, ['encrypt','decrypt'])")>]
let private importKey (key: byte[]) : JS.Promise<obj> = jsNative

[<Emit("globalThis.crypto.subtle.encrypt({name:'AES-CBC', iv:Uint8Array.from($1)}, $0, Uint8Array.from($2)).then(function(b){ return new Uint8Array(b); })")>]
let private encryptPadded (key: obj) (iv: byte[]) (data: byte[]) : JS.Promise<byte[]> = jsNative

[<Emit("globalThis.crypto.subtle.decrypt({name:'AES-CBC', iv:Uint8Array.from($1)}, $0, Uint8Array.from($2)).then(function(b){ return new Uint8Array(b); })")>]
let private decryptPadded (key: obj) (iv: byte[]) (data: byte[]) : JS.Promise<byte[]> = jsNative

let private sub (data: byte[]) start len = Array.sub data start len

/// Raw AES-CBC encryption: the padded ciphertext without its final (padding) block.
let private aesEncrypt (key: byte[]) (iv: byte[]) (data: byte[]) = async {
    let! k = importKey key |> Async.AwaitPromise
    let! padded = encryptPadded k iv data |> Async.AwaitPromise
    return sub padded 0 data.Length }

/// Raw AES-CBC decryption: append the block that encrypts a full PKCS#7 pad
/// after the last ciphertext block, so WebCrypto's padding check passes and
/// strips exactly that block.
let private aesDecrypt (key: byte[]) (iv: byte[]) (data: byte[]) = async {
    if data.Length = 0 then return Array.empty
    else
        let! k = importKey key |> Async.AwaitPromise
        let last = sub data (data.Length - 16) 16
        let! padBlock = encryptPadded k last (Array.create 16 16uy) |> Async.AwaitPromise
        let! plain = decryptPadded k iv (Array.append data (sub padBlock 0 16)) |> Async.AwaitPromise
        return plain }

// ---- MD5 (RFC 1321) ----

let private md5Shifts =
    [| 7; 12; 17; 22; 7; 12; 17; 22; 7; 12; 17; 22; 7; 12; 17; 22
       5; 9; 14; 20; 5; 9; 14; 20; 5; 9; 14; 20; 5; 9; 14; 20
       4; 11; 16; 23; 4; 11; 16; 23; 4; 11; 16; 23; 4; 11; 16; 23
       6; 10; 15; 21; 6; 10; 15; 21; 6; 10; 15; 21; 6; 10; 15; 21 |]

let private md5Table =
    Array.init 64 (fun i -> uint32 (int64 (abs (sin (float (i + 1))) * 4294967296.0)))

let md5 (input: byte[]) : byte[] =
    let bitLength = uint64 input.Length * 8UL
    let paddedLength = ((input.Length + 8) / 64 + 1) * 64
    let msg = Array.zeroCreate<byte> paddedLength
    Array.blit input 0 msg 0 input.Length
    msg.[input.Length] <- 0x80uy
    for i in 0 .. 7 do
        msg.[paddedLength - 8 + i] <- byte ((bitLength >>> (8 * i)) &&& 0xFFUL)
    let mutable a0 = 0x67452301u
    let mutable b0 = 0xefcdab89u
    let mutable c0 = 0x98badcfeu
    let mutable d0 = 0x10325476u
    let m = Array.zeroCreate<uint32> 16
    for chunk in 0 .. paddedLength / 64 - 1 do
        for i in 0 .. 15 do
            let o = chunk * 64 + i * 4
            m.[i] <- uint32 msg.[o] ||| (uint32 msg.[o + 1] <<< 8) ||| (uint32 msg.[o + 2] <<< 16) ||| (uint32 msg.[o + 3] <<< 24)
        let mutable a = a0
        let mutable b = b0
        let mutable c = c0
        let mutable d = d0
        for i in 0 .. 63 do
            let f, g =
                if i < 16 then (b &&& c) ||| (~~~b &&& d), i
                elif i < 32 then (d &&& b) ||| (~~~d &&& c), (5 * i + 1) % 16
                elif i < 48 then b ^^^ c ^^^ d, (3 * i + 5) % 16
                else c ^^^ (b ||| ~~~d), (7 * i) % 16
            let f = f + a + md5Table.[i] + m.[g]
            a <- d
            d <- c
            c <- b
            let s = md5Shifts.[i]
            b <- b + ((f <<< s) ||| (f >>> (32 - s)))
        a0 <- a0 + a
        b0 <- b0 + b
        c0 <- c0 + c
        d0 <- d0 + d
    [| for v in [ a0; b0; c0; d0 ] do
         for i in 0 .. 3 do yield byte ((v >>> (8 * i)) &&& 0xFFu) |]

let provider : Encryption.Crypto =
    { Md5 = fun data -> async { return md5 data }
      Sha256 = fun data -> digest "SHA-256" data |> Async.AwaitPromise
      Sha384 = fun data -> digest "SHA-384" data |> Async.AwaitPromise
      Sha512 = fun data -> digest "SHA-512" data |> Async.AwaitPromise
      AesCbcEncrypt = aesEncrypt
      AesCbcDecrypt = aesDecrypt }

/// The PDF unencrypted (opened with no password), or as it is when it is not
/// encrypted.
let decryptIfNeeded (bytes: byte[]) : JS.Promise<byte[]> =
    async {
        let! result = Encryption.decrypt provider "" bytes
        return result.Data }
    |> Async.StartAsPromise
