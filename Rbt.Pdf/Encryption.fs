namespace Rbt.Pdf

open System
open System.Text

/// Opening encrypted PDFs (the standard security handler, ISO 32000-2 7.6).
///
/// Rbt.Pdf carries no hash or cipher code of its own: the host passes a
/// `Crypto` with MD5, SHA-2 and raw AES-CBC (System.Security.Cryptography on
/// .NET, WebCrypto in a browser). Each primitive returns Async, so an async
/// provider such as WebCrypto plugs in directly. RC4, which neither platform
/// offers, is implemented here.
///
/// `decrypt` turns an encrypted file into an unencrypted one; every other
/// Rbt.Pdf API then works on the result synchronously. Callers should honour
/// `Permissions` (the /P flags the author set) for what they do next.
module Encryption =

    /// Hash and cipher primitives supplied by the host.
    type Crypto = {
        Md5: byte[] -> Async<byte[]>
        Sha256: byte[] -> Async<byte[]>
        Sha384: byte[] -> Async<byte[]>
        Sha512: byte[] -> Async<byte[]>
        /// AES-CBC encryption without padding: key (16 or 32 bytes), IV
        /// (16 bytes), data a multiple of 16 bytes.
        AesCbcEncrypt: byte[] -> byte[] -> byte[] -> Async<byte[]>
        /// AES-CBC decryption without padding removal: key, IV, data a
        /// multiple of 16 bytes.
        AesCbcDecrypt: byte[] -> byte[] -> byte[] -> Async<byte[]>
    }

    /// The /P permission bits (ISO 32000-2 table 22), named.
    type Permissions = {
        Raw: int
        Print: bool
        Modify: bool
        Copy: bool
        Annotate: bool
        FillForms: bool
        Assemble: bool
    }

    type DecryptResult = {
        /// The same document, unencrypted.
        Data: byte[]
        Permissions: Permissions
        /// Whether `password` matched the owner password (all permissions).
        Owner: bool
    }

    let permissions (p: int) =
        let bit n = p &&& (1 <<< (n - 1)) <> 0
        { Raw = p
          Print = bit 3
          Modify = bit 4
          Copy = bit 5
          Annotate = bit 6
          // Bit 9 grants filling alone; bit 6 (annotate) includes it.
          FillForms = bit 9 || bit 6
          Assemble = bit 11 || bit 4 }

    let private allPermissions = permissions -1

    /// True when the document uses encryption.
    let isEncrypted (doc: PdfDocument) = doc.Trailer.ContainsKey "Encrypt"

    // ---- RC4 (not offered by WebCrypto or .NET) ----

    let rc4 (key: byte[]) (data: byte[]) =
        let s = Array.init 256 id
        let mutable j = 0
        for i in 0 .. 255 do
            j <- (j + s.[i] + int key.[i % key.Length]) &&& 0xFF
            let t = s.[i]
            s.[i] <- s.[j]
            s.[j] <- t
        let out = Array.zeroCreate<byte> data.Length
        let mutable i = 0
        j <- 0
        for k in 0 .. data.Length - 1 do
            i <- (i + 1) &&& 0xFF
            j <- (j + s.[i]) &&& 0xFF
            let t = s.[i]
            s.[i] <- s.[j]
            s.[j] <- t
            out.[k] <- data.[k] ^^^ byte s.[(s.[i] + s.[j]) &&& 0xFF]
        out

    // ---- Handler parameters ----

    type private Method =
        | Identity
        | Rc4
        | Aes128
        | Aes256

    type private Handler = {
        V: int
        R: int
        KeyLength: int
        O: byte[]
        U: byte[]
        OE: byte[]
        UE: byte[]
        P: int
        Id0: byte[]
        EncryptMetadata: bool
        StreamMethod: Method
        StringMethod: Method
    }

    let private padding =
        [| 0x28uy; 0xBFuy; 0x4Euy; 0x5Euy; 0x4Euy; 0x75uy; 0x8Auy; 0x41uy
           0x64uy; 0x00uy; 0x4Euy; 0x56uy; 0xFFuy; 0xFAuy; 0x01uy; 0x08uy
           0x2Euy; 0x2Euy; 0x00uy; 0xB6uy; 0xD0uy; 0x68uy; 0x3Euy; 0x80uy
           0x2Fuy; 0x0Cuy; 0xA9uy; 0xFEuy; 0x64uy; 0x53uy; 0x69uy; 0x7Auy |]

    let private int32Le (v: int) =
        [| byte (v &&& 0xFF); byte ((v >>> 8) &&& 0xFF); byte ((v >>> 16) &&& 0xFF); byte ((v >>> 24) &&& 0xFF) |]

    let private take n (data: byte[]) =
        if data.Length <= n then data else Array.sub data 0 n

    let private range start len (data: byte[]) =
        if data.Length < start + len then failwith "The encryption dictionary is malformed."
        Array.sub data start len

    let private handlerOf (doc: PdfDocument) : Handler =
        let enc =
            match Map.tryFind "Encrypt" doc.Trailer |> Option.map (Pdf.deref doc) with
            | Some (PdfDict d) -> d
            | _ -> failwith "The encryption dictionary is missing."
        let get k = Map.tryFind k enc |> Option.map (Pdf.deref doc)
        let int' k d = match get k with Some (PdfInt n) -> int n | _ -> d
        let bytes k = match get k with Some (PdfString b) -> b | _ -> Array.empty
        match get "Filter" with
        | Some (PdfName "Standard") -> ()
        | Some (PdfName f) -> failwithf "The PDF uses the %s security handler; only passwords (Standard) are supported." f
        | _ -> failwith "The encryption dictionary has no /Filter."
        let v = int' "V" 0
        let r = int' "R" 2
        let cryptFilter (name: string) =
            if name = "Identity" then Identity
            else
                let cf =
                    match get "CF" with
                    | Some (PdfDict cf) ->
                        match Map.tryFind name cf |> Option.map (Pdf.deref doc) with
                        | Some (PdfDict f) -> f
                        | _ -> Map.empty
                    | _ -> Map.empty
                match Map.tryFind "CFM" cf with
                | Some (PdfName "V2") -> Rc4
                | Some (PdfName "AESV2") -> Aes128
                | Some (PdfName "AESV3") -> Aes256
                | Some (PdfName "None") | None -> Identity
                | Some other -> failwithf "Unsupported crypt filter method %A." other
        let filterNamed k =
            match get k with
            | Some (PdfName n) -> n
            | _ -> "Identity"
        let streamMethod, stringMethod =
            if v >= 4 then cryptFilter (filterNamed "StmF"), cryptFilter (filterNamed "StrF")
            elif v = 1 || v = 2 || v = 3 then Rc4, Rc4
            else failwithf "Unsupported encryption version V=%d." v
        let keyLength =
            match v with
            | 1 -> 5
            | 5 -> 32
            | 4 ->
                // A V4 key's length comes from the crypt filter (bytes, or bits in some files).
                match get "CF" with
                | Some (PdfDict cf) ->
                    let lens =
                        cf |> Map.toList |> List.choose (fun (_, f) ->
                            match Pdf.deref doc f with
                            | PdfDict f -> (match Map.tryFind "Length" f with Some (PdfInt n) -> Some (int n) | _ -> None)
                            | _ -> None)
                    match lens with
                    | n :: _ when n > 32 -> n / 8
                    | n :: _ when n >= 5 -> n
                    | _ -> 16
                | _ -> 16
            | _ -> max 5 (min 16 (int' "Length" 40 / 8))
        let id0 =
            match Map.tryFind "ID" doc.Trailer |> Option.map (Pdf.deref doc) with
            | Some (PdfArray (first :: _)) -> (match Pdf.deref doc first with PdfString b -> b | _ -> Array.empty)
            | _ -> Array.empty
        { V = v
          R = r
          KeyLength = keyLength
          O = bytes "O"
          U = bytes "U"
          OE = bytes "OE"
          UE = bytes "UE"
          P = int' "P" -1
          Id0 = id0
          EncryptMetadata = (match get "EncryptMetadata" with Some (PdfBool false) -> false | _ -> true)
          StreamMethod = streamMethod
          StringMethod = stringMethod }

    // ---- File key, revisions 2-4 (ISO 32000-2 7.6.4.3) ----

    let private padPassword (password: byte[]) =
        Array.append (take 32 password) (Array.sub padding 0 (32 - min 32 password.Length))

    let private md5Iterated (crypto: Crypto) (h: Handler) (input: byte[]) = async {
        let! first = crypto.Md5 input
        let mutable hash = first
        if h.R >= 3 then
            for _ in 1 .. 50 do
                let! next = crypto.Md5 (take h.KeyLength hash)
                hash <- next
        return take h.KeyLength hash }

    /// Algorithm 2: the file key from a (padded) user password.
    let private legacyKey (crypto: Crypto) (h: Handler) (userPassword: byte[]) =
        let input =
            Array.concat [
                padPassword userPassword
                h.O
                int32Le h.P
                h.Id0
                (if h.R >= 4 && not h.EncryptMetadata then [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |] else Array.empty) ]
        md5Iterated crypto h input

    /// Algorithms 4 and 5: whether `key` reproduces /U.
    let private legacyUserMatches (crypto: Crypto) (h: Handler) (key: byte[]) = async {
        if h.R = 2 then
            return rc4 key padding = take 32 h.U
        else
            let! hash = crypto.Md5 (Array.append padding h.Id0)
            let mutable x = rc4 key hash
            for i in 1 .. 19 do
                x <- rc4 (key |> Array.map (fun b -> b ^^^ byte i)) x
            return h.U.Length >= 16 && x = Array.sub h.U 0 16 }

    /// Algorithm 7: the user password recovered from /O with an owner password.
    let private legacyUserFromOwner (crypto: Crypto) (h: Handler) (ownerPassword: byte[]) = async {
        let! key = md5Iterated crypto h (padPassword ownerPassword)
        if h.R = 2 then return rc4 key (take 32 h.O)
        else
            let mutable x = take 32 h.O
            for i in 19 .. -1 .. 0 do
                x <- rc4 (key |> Array.map (fun b -> b ^^^ byte i)) x
            return x }

    // ---- File key, revisions 5 and 6 (ISO 32000-2 7.6.4.3.3) ----

    /// Algorithm 2.B (R6), or a single SHA-256 (R5).
    let private hash2B (crypto: Crypto) (r: int) (password: byte[]) (salt: byte[]) (udata: byte[]) = async {
        let! initial = crypto.Sha256 (Array.concat [ password; salt; udata ])
        if r < 6 then return initial
        else
            let mutable k = initial
            let mutable round = 0
            let mutable fin = false
            while not fin do
                let k1 = Array.concat [ password; k; udata ]
                let block = Array.concat (Array.create 64 k1)
                let! e = crypto.AesCbcEncrypt (Array.sub k 0 16) (Array.sub k 16 16) block
                // The first 16 bytes of E as a big-endian number, mod 3: 256 = 1 (mod 3).
                let n = (Array.sub e 0 16 |> Array.sumBy int) % 3
                let! next =
                    match n with
                    | 0 -> crypto.Sha256 e
                    | 1 -> crypto.Sha384 e
                    | _ -> crypto.Sha512 e
                k <- next
                round <- round + 1
                if round >= 64 && int e.[e.Length - 1] <= round - 32 then fin <- true
            return Array.sub k 0 32 }

    let private aes256Key (crypto: Crypto) (h: Handler) (password: byte[]) = async {
        let password = take 127 password
        if h.U.Length < 48 || h.O.Length < 48 then failwith "The encryption dictionary is malformed."
        let u48 = Array.sub h.U 0 48
        let! ownerCheck = hash2B crypto h.R password (range 32 8 h.O) u48
        if ownerCheck = Array.sub h.O 0 32 then
            let! ik = hash2B crypto h.R password (range 40 8 h.O) u48
            let! key = crypto.AesCbcDecrypt ik (Array.zeroCreate 16) (range 0 32 h.OE)
            return Some (key, true)
        else
            let! userCheck = hash2B crypto h.R password (range 32 8 h.U) Array.empty
            if userCheck = Array.sub h.U 0 32 then
                let! ik = hash2B crypto h.R password (range 40 8 h.U) Array.empty
                let! key = crypto.AesCbcDecrypt ik (Array.zeroCreate 16) (range 0 32 h.UE)
                return Some (key, false)
            else return None }

    /// The file key and whether the password was the owner's, or None when it
    /// is neither password.
    let private fileKey (crypto: Crypto) (h: Handler) (password: string) = async {
        if h.R >= 5 then
            return! aes256Key crypto h (Encoding.UTF8.GetBytes password)
        else
            let pw = latin1Bytes password
            let! fromOwner = legacyUserFromOwner crypto h pw
            let! ownerKey = legacyKey crypto h fromOwner
            let! isOwner = legacyUserMatches crypto h ownerKey
            if isOwner then return Some (ownerKey, true)
            else
                let! userKey = legacyKey crypto h pw
                let! isUser = legacyUserMatches crypto h userKey
                return if isUser then Some (userKey, false) else None }

    // ---- Strings and streams ----

    /// Algorithm 1: the key for one object (the file key itself for AES-256).
    let private objectKey (crypto: Crypto) (h: Handler) (fileKey: byte[]) (method: Method) (num: int) (gen: int) = async {
        if method = Aes256 then return fileKey
        else
            let input =
                Array.concat [
                    fileKey
                    [| byte (num &&& 0xFF); byte ((num >>> 8) &&& 0xFF); byte ((num >>> 16) &&& 0xFF)
                       byte (gen &&& 0xFF); byte ((gen >>> 8) &&& 0xFF) |]
                    (if method = Aes128 then "sAlT"B else Array.empty) ]
            let! hash = crypto.Md5 input
            return take (min 16 (h.KeyLength + 5)) hash }

    let private decryptBytes (crypto: Crypto) (method: Method) (key: byte[]) (data: byte[]) = async {
        match method with
        | Identity -> return data
        | Rc4 -> return rc4 key data
        | Aes128 | Aes256 ->
            if data.Length < 32 then return Array.empty  // an IV and no whole block (an empty value)
            else
                let iv = Array.sub data 0 16
                let body = Array.sub data 16 ((data.Length - 16) / 16 * 16)
                let! plain = crypto.AesCbcDecrypt key iv body
                // PKCS#7 padding; leave the bytes as they are when it is not valid.
                let pad = if plain.Length > 0 then int plain.[plain.Length - 1] else 0
                let valid =
                    pad >= 1 && pad <= 16 && pad <= plain.Length
                    && Array.forall (fun b -> int b = pad) (Array.sub plain (plain.Length - pad) pad)
                return if valid then Array.sub plain 0 (plain.Length - pad) else plain }

    let rec private decryptStrings (decrypt: byte[] -> Async<byte[]>) (obj: PdfObj) : Async<PdfObj> = async {
        match obj with
        | PdfString b ->
            let! plain = decrypt b
            return PdfString plain
        | PdfArray items ->
            let out = ResizeArray<PdfObj>()
            for item in items do
                let! item = decryptStrings decrypt item
                out.Add item
            return PdfArray (List.ofSeq out)
        | PdfDict d ->
            let! d = decryptMap decrypt d
            return PdfDict d
        | PdfStream (h, data) ->
            let! h = decryptMap decrypt h
            return PdfStream (h, data)
        | other -> return other }

    and private decryptMap decrypt (d: Map<string, PdfObj>) = async {
        let mutable out = Map.empty
        for KeyValue (k, v) in d do
            let! v = decryptStrings decrypt v
            out <- Map.add k v out
        return out }

    let private typeName (h: Map<string, PdfObj>) =
        match Map.tryFind "Type" h with
        | Some (PdfName t) -> t
        | _ -> ""

    // ---- Writing the unencrypted file ----

    let private rewrite (doc: PdfDocument) (header: string) (objects: (int * int * PdfObj) list) =
        let sb = StringBuilder()
        sb.Append(header).Append("\n%\xe2\xe3\xcf\xd3\n") |> ignore
        let positions = Collections.Generic.Dictionary<int, int * int>()
        for num, gen, obj in objects do
            positions.[num] <- (sb.Length, gen)
            sb.Append(num).Append(' ').Append(gen).Append(" obj\n") |> ignore
            Output.serializeTo sb obj
            sb.Append("\nendobj\n") |> ignore
        let size = (objects |> List.fold (fun m (n, _, _) -> max m n) 0) + 1
        let xrefPos = sb.Length
        sb.Append("xref\n0 ").Append(size).Append('\n') |> ignore
        sb.Append("0000000000 65535 f \n") |> ignore
        for n in 1 .. size - 1 do
            match positions.TryGetValue n with
            | true, (pos, gen) ->
                sb.Append(pos.ToString().PadLeft(10, '0')).Append(' ').Append(gen.ToString().PadLeft(5, '0')).Append(" n \n") |> ignore
            | _ -> sb.Append("0000000000 00000 f \n") |> ignore
        let trailer =
            doc.Trailer
            |> Map.filter (fun k _ -> k = "Info" || k = "ID")
            |> Map.add "Root" doc.Catalog
            |> Map.add "Size" (PdfInt (int64 size))
        sb.Append("trailer\n") |> ignore
        Output.serializeTo sb (PdfDict trailer)
        sb.Append("\nstartxref\n").Append(xrefPos).Append("\n%%EOF\n") |> ignore
        latin1Bytes (sb.ToString())

    /// The document unencrypted, opened with `password` (the user or owner
    /// password; "" opens a file that has no user password). Fails when the
    /// password is wrong or the file uses an unsupported security handler.
    /// An unencrypted file is returned as it is, with every permission.
    let decrypt (crypto: Crypto) (password: string) (data: byte[]) : Async<DecryptResult> = async {
        let doc = Pdf.openData data
        if not (isEncrypted doc) then
            return { Data = data; Permissions = allPermissions; Owner = true }
        else
            let h = handlerOf doc
            let! key = fileKey crypto h password
            match key with
            | None ->
                return failwith (if password = "" then "The PDF needs a password to open." else "The password is not correct for this PDF.")
            | Some (fileKey, owner) ->
                let encryptNum =
                    match Map.tryFind "Encrypt" doc.Trailer with
                    | Some (PdfIndirect (n, _)) -> n
                    | _ -> -1
                // Objects stored directly in the file are encrypted; objects in
                // object streams are not (their stream is). Decrypt the direct
                // objects first so object streams then decode.
                let original = Array.copy doc.XRef
                let decrypted = Collections.Generic.Dictionary<int, int * PdfObj>()
                for num in 1 .. original.Length - 1 do
                    match original.[num] with
                    | InUse (pos, gen, _) when num <> encryptNum ->
                        let obj = try Pdf.deref doc (PdfIndirect (num, gen)) with _ -> PdfNull
                        let! strKey = objectKey crypto h fileKey h.StringMethod num gen
                        let strings = decryptBytes crypto h.StringMethod strKey
                        let! obj =
                            match obj with
                            | PdfStream (header, raw) when typeName header = "XRef" ->
                                async { return PdfStream (header, raw) }
                            | PdfStream (header, raw) ->
                                async {
                                    let! header = decryptMap strings header
                                    let skip = typeName header = "Metadata" && not h.EncryptMetadata
                                    let! stmKey = objectKey crypto h fileKey h.StreamMethod num gen
                                    let! plain = if skip then async { return raw } else decryptBytes crypto h.StreamMethod stmKey raw
                                    return PdfStream (header, plain) }
                            | other -> decryptStrings strings other
                        decrypted.[num] <- (gen, obj)
                        doc.XRef.[num] <- InUse (pos, gen, Some obj)
                    | _ -> ()
                // deref may have cached compressed objects (read from still
                // encrypted object streams) while resolving lengths: reread them.
                for num in 1 .. original.Length - 1 do
                    match original.[num] with
                    | Compressed _ as e -> doc.XRef.[num] <- e
                    | _ -> ()
                let objects = ResizeArray<int * int * PdfObj>()
                for num in 1 .. original.Length - 1 do
                    match original.[num] with
                    | InUse _ ->
                        match decrypted.TryGetValue num with
                        | true, (gen, obj) ->
                            match obj with
                            | PdfStream (header, _) when (let t = typeName header in t = "XRef" || t = "ObjStm") -> ()
                            | _ -> objects.Add((num, gen, obj))
                        | _ -> ()
                    | Compressed _ ->
                        let obj = try Pdf.deref doc (PdfIndirect (num, 0)) with _ -> PdfNull
                        objects.Add((num, 0, obj))
                    | Free _ -> ()
                let header =
                    let line = bytesToAsciiRange doc.Data 0 (min 8 doc.Data.Length)
                    if line.StartsWith "%PDF-" then line.TrimEnd('\r', '\n', ' ') else "%PDF-1.7"
                return {
                    Data = rewrite doc header (List.ofSeq objects)
                    Permissions = if owner then allPermissions else permissions h.P
                    Owner = owner }
    }
