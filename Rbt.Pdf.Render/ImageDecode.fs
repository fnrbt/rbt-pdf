/// Synchronous image decoder wrapper for PDF images.
/// Uses the img-decoder binary via Unix socketpair + memfd shared memory.
module Rbt.Pdf.Render.ImageDecode

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

#nowarn "9"

// ── Native imports ──

[<DllImport("libc", SetLastError = true, EntryPoint = "syscall")>]
extern int private memfd_create_syscall(nativeint nr, nativeint name, uint32 flags)

[<DllImport("libc", SetLastError = true)>]
extern int private ftruncate(int fd, int64 length)

[<DllImport("libc", SetLastError = true)>]
extern nativeint private mmap(nativeint addr, unativeint length, int prot, int flags, int fd, int64 offset)

[<DllImport("libc", SetLastError = true)>]
extern int private nativeClose(int fd)

[<DllImport("libc", SetLastError = true)>]
extern int private socketpair(int domain, int typ, int protocol, int[] sv)

[<DllImport("libc", SetLastError = true)>]
extern nativeint private nativeRead(int fd, nativeint buf, unativeint count)

[<DllImport("libc", SetLastError = true)>]
extern nativeint private nativeWrite(int fd, nativeint buf, unativeint count)

[<DllImport("libc")>]
extern int private posix_spawn_file_actions_init(nativeint fa)

[<DllImport("libc")>]
extern int private posix_spawn_file_actions_destroy(nativeint fa)

[<DllImport("libc")>]
extern int private posix_spawn_file_actions_addclose(nativeint fa, int fd)

[<DllImport("libc")>]
extern int private posix_spawn(int& pid, nativeint path, nativeint fa, nativeint attrp, nativeint argv, nativeint envp)

let private SYS_MEMFD_CREATE = 319n
let private AF_UNIX = 1
let private SOCK_STREAM = 1
let private PROT_READ = 0x1
let private PROT_WRITE = 0x2
let private MAP_SHARED = 0x01
let private SLOT_SIZE = 16 * 1024 * 1024  // 16 MB

let private memfdCreate (name: string) =
    let nameBytes = Text.Encoding.UTF8.GetBytes(name + "\x00")
    let namePin = GCHandle.Alloc(nameBytes, GCHandleType.Pinned)
    try memfd_create_syscall(SYS_MEMFD_CREATE, namePin.AddrOfPinnedObject(), 0u)
    finally namePin.Free()

let private readExact (fd: int) (ptr: nativeint) (count: int) =
    let mutable off = 0
    while off < count do
        let n = int (nativeRead(fd, ptr + nativeint off, unativeint (count - off)))
        if n <= 0 then failwith "read: connection closed"
        off <- off + n

let private writeAll (fd: int) (ptr: nativeint) (count: int) =
    let mutable off = 0
    while off < count do
        let n = int (nativeWrite(fd, ptr + nativeint off, unativeint (count - off)))
        if n <= 0 then failwith "write: connection closed"
        off <- off + n

let private allocCStringArray (strs: string[]) : GCHandle * nativeint[] =
    let ptrs = Array.zeroCreate<nativeint> (strs.Length + 1)
    for i in 0 .. strs.Length - 1 do
        ptrs.[i] <- Marshal.StringToHGlobalAnsi(strs.[i])
    let pin = GCHandle.Alloc(ptrs, GCHandleType.Pinned)
    (pin, ptrs)

let private freeCStringArray (pin: GCHandle) (ptrs: nativeint[]) =
    if pin.IsAllocated then pin.Free()
    for p in ptrs do
        if p <> 0n then Marshal.FreeHGlobal(p)

let private allocEnvp () : GCHandle * nativeint[] =
    let vars = Environment.GetEnvironmentVariables()
    let entries = ResizeArray<string>()
    for entry in vars do
        let de = entry :?> Collections.DictionaryEntry
        entries.Add(sprintf "%O=%O" de.Key de.Value)
    allocCStringArray (entries.ToArray())

let private spawnProcess (exePath: string) (args: string[]) (closeFd: int) : int =
    let faPtr = Marshal.AllocHGlobal(128)
    NativePtr.initBlock (NativePtr.ofNativeInt<byte> faPtr) 0uy 128u
    posix_spawn_file_actions_init(faPtr) |> ignore
    posix_spawn_file_actions_addclose(faPtr, closeFd) |> ignore
    let pathPtr = Marshal.StringToHGlobalAnsi(exePath)
    let (argvPin, argvPtrs) = allocCStringArray args
    let (envpPin, envpPtrs) = allocEnvp ()
    try
        let mutable pid = 0
        let ret = posix_spawn(&pid, pathPtr, faPtr, 0n, argvPin.AddrOfPinnedObject(), envpPin.AddrOfPinnedObject())
        if ret <> 0 then failwithf "posix_spawn failed: %d" ret
        pid
    finally
        posix_spawn_file_actions_destroy(faPtr) |> ignore
        Marshal.FreeHGlobal(faPtr)
        Marshal.FreeHGlobal(pathPtr)
        freeCStringArray argvPin argvPtrs
        freeCStringArray envpPin envpPtrs

// ── Protocol structs ──

[<Struct; StructLayout(LayoutKind.Sequential, Pack = 1, Size = 16)>]
type private DecodeRequest =
    val mutable Op: byte
    val mutable Fit: byte
    val mutable TargetW: uint16
    val mutable TargetH: uint16
    val mutable Flags: byte
    val mutable Slot: byte
    val mutable CropX: byte
    val mutable CropY: byte
    val mutable Reserved: uint16
    val mutable PathLen: uint32

[<Struct; StructLayout(LayoutKind.Sequential, Pack = 1, Size = 8)>]
type private DecodeResponse =
    val mutable Status: byte
    val mutable Slot: byte
    val mutable Width: uint16
    val mutable Height: uint16
    val mutable ErrLen: uint16

// ── State ──

let mutable private sockFd = -1
let mutable private poolPtr = 0n
let mutable private initialized = false
let private initLock = obj()

let private findDecoder () =
    let p = IO.Path.GetFullPath(IO.Path.Combine(__SOURCE_DIRECTORY__, "../../browser-browser/decoder/img-decoder"))
    if IO.File.Exists(p) then p
    else
        let envPath = Environment.GetEnvironmentVariable("BROWSER_DECODER_PATH")
        if not (String.IsNullOrEmpty(envPath)) && IO.File.Exists(envPath) then envPath
        else "img-decoder"

let private ensureInit () =
    if not initialized then
        lock initLock (fun () ->
            if not initialized then
                try
                    let decoderPath = findDecoder ()
                    let sv = Array.zeroCreate<int> 2
                    if socketpair(AF_UNIX, SOCK_STREAM, 0, sv) < 0 then
                        failwith "socketpair failed"
                    let sockBrowser = sv.[0]
                    let sockWorker = sv.[1]

                    let fd = memfdCreate "pdf-img-pool"
                    if fd < 0 then failwith "memfd_create failed"
                    let poolSize = int64 SLOT_SIZE
                    if ftruncate(fd, poolSize) < 0 then
                        nativeClose(fd) |> ignore
                        failwith "ftruncate failed"
                    let ptr = mmap(0n, unativeint poolSize, PROT_READ ||| PROT_WRITE, MAP_SHARED, fd, 0L)
                    if ptr = -1n then
                        nativeClose(fd) |> ignore
                        failwith "mmap failed"

                    let args = [| decoderPath; string sockWorker; string fd; "1" |]
                    let _pid = spawnProcess decoderPath args sockBrowser
                    nativeClose(sockWorker) |> ignore

                    sockFd <- sockBrowser
                    poolPtr <- ptr
                    initialized <- true
                with ex ->
                    eprintfn "[pdf-img-decoder] init failed: %s" ex.Message)

/// Decode image bytes (JPEG/PNG) to BGRA pixels.
/// Returns (width, height, pixels) or None on failure.
let decode (imageBytes: byte[]) : (int * int * uint32[]) option =
    try
        ensureInit ()
        if sockFd < 0 then None
        else

        let tmpPath = IO.Path.GetTempFileName()
        try
            IO.File.WriteAllBytes(tmpPath, imageBytes)
            let pathBytes = Text.Encoding.UTF8.GetBytes(tmpPath)

            let reqBuf = Array.zeroCreate<byte> 16
            let reqPin = GCHandle.Alloc(reqBuf, GCHandleType.Pinned)
            let reqPtr = reqPin.AddrOfPinnedObject()
            let mutable req = DecodeRequest()
            req.Op <- 1uy
            req.Fit <- 3uy
            req.TargetW <- 0us
            req.TargetH <- 0us
            req.Flags <- 0uy
            req.Slot <- 0uy
            req.CropX <- 0uy
            req.CropY <- 0uy
            req.Reserved <- 0us
            req.PathLen <- uint32 pathBytes.Length
            Marshal.StructureToPtr(req, reqPtr, false)

            let pathBuf = Array.zeroCreate<byte> (max pathBytes.Length 1)
            Buffer.BlockCopy(pathBytes, 0, pathBuf, 0, pathBytes.Length)
            let pathPin = GCHandle.Alloc(pathBuf, GCHandleType.Pinned)

            try
                writeAll sockFd reqPtr 16
                writeAll sockFd (pathPin.AddrOfPinnedObject()) pathBytes.Length

                let respBuf = Array.zeroCreate<byte> 8
                let respPin = GCHandle.Alloc(respBuf, GCHandleType.Pinned)
                let respPtr = respPin.AddrOfPinnedObject()
                try
                    readExact sockFd respPtr 8
                    let resp = Marshal.PtrToStructure<DecodeResponse>(respPtr)

                    if resp.ErrLen > 0us then
                        let errBuf = Array.zeroCreate<byte> (int resp.ErrLen)
                        let errPin = GCHandle.Alloc(errBuf, GCHandleType.Pinned)
                        try readExact sockFd (errPin.AddrOfPinnedObject()) (int resp.ErrLen)
                        finally if errPin.IsAllocated then errPin.Free()

                    if resp.Status = 0uy then
                        let w = int resp.Width
                        let h = int resp.Height
                        let pixelCount = w * h
                        let pixels = Array.zeroCreate<uint32> pixelCount
                        let src = Span<uint32>(poolPtr.ToPointer(), pixelCount)
                        src.CopyTo(Span<uint32>(pixels))
                        Some (w, h, pixels)
                    else None
                finally
                    if respPin.IsAllocated then respPin.Free()
            finally
                if pathPin.IsAllocated then pathPin.Free()
                if reqPin.IsAllocated then reqPin.Free()
        finally
            try IO.File.Delete(tmpPath) with _ -> ()
    with _ -> None
