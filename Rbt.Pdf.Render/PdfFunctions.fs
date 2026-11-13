module internal Rbt.Pdf.Render.PdfFunctions

open System
open Rbt.Pdf

let private clamp lo hi x = max lo (min hi x)
let private finite x = not (Double.IsNaN x || Double.IsInfinity x)

let private number doc obj =
    match Pdf.deref doc obj with
    | PdfInt n -> Some (float n)
    | PdfReal n when finite n -> Some n
    | _ -> None

let private sequence xs =
    let rec loop acc = function
        | [] -> Some (List.rev acc)
        | Some x :: rest -> loop (x :: acc) rest
        | None :: _ -> None
    loop [] xs

let private numbers doc obj =
    match Pdf.deref doc obj with
    | PdfArray xs -> xs |> List.map (number doc) |> sequence |> Option.map List.toArray
    | _ -> None

let private field doc key obj = Pdf.dictGet doc key obj
let private numberField doc key obj = field doc key obj |> Option.bind (number doc)
let private arrayField doc key obj = field doc key obj |> Option.bind (numbers doc)

exception FunctionEvaluationError

type Function =
    { Eval: float -> float[]
      EvalN: float[] -> float[]
      Inputs: int
      Outputs: int
      Breaks: float list }

let rec private parseFunction doc depth obj : Function option =
    if depth > 16 then None else
    let validPairs (a: float[]) =
        a.Length > 0 && a.Length % 2 = 0 &&
        ([0 .. 2 .. a.Length - 2] |> List.forall (fun i -> a.[i] <= a.[i+1]))
    let domain = arrayField doc "Domain" obj
    let range = match field doc "Range" obj with None -> Some None | Some x -> numbers doc x |> Option.map Some
    match domain, range with
    | Some d, Some r when validPairs d && (r |> Option.forall validPairs) ->
        let inputs = d.Length / 2
        let wrap outputs (eval: float[] -> float[] -> bool) breaks =
            let clipped = Array.zeroCreate<float> inputs
            let values = Array.zeroCreate<float> outputs
            let single = Array.zeroCreate<float> 1
            let run (xs: float[]) =
                if xs.Length <> inputs then raise FunctionEvaluationError
                for i in 0 .. inputs-1 do
                    clipped.[i] <- clamp d.[2*i] d.[2*i+1] xs.[i]
                if not (eval clipped values) || not (Array.forall finite values) then raise FunctionEvaluationError
                match r with
                | Some limits ->
                    for i in 0 .. outputs-1 do values.[i] <- clamp limits.[2*i] limits.[2*i+1] values.[i]
                | None -> ()
                values
            { Eval = (fun x -> single.[0] <- x; run single)
              EvalN = run; Inputs = inputs; Outputs = outputs; Breaks = breaks }
        match numberField doc "FunctionType" obj with
        | Some 0.0 ->
            match r, arrayField doc "Size" obj, numberField doc "BitsPerSample" obj with
            | Some limits, Some sizes, Some bits when
                sizes.Length = inputs && Array.forall (fun n -> n >= 1.0 && n <= float Int32.MaxValue && n = floor n) sizes &&
                bits = floor bits && bits >= 0.0 && bits <= 32.0 ->
                let size = sizes |> Array.map int
                let encode = match field doc "Encode" obj with None -> Some (Array.init (inputs*2) (fun i -> if i % 2 = 0 then 0.0 else float (size.[i/2]-1))) | Some x -> numbers doc x
                let decode = match field doc "Decode" obj with None -> Some limits | Some x -> numbers doc x
                let order = match field doc "Order" obj with None -> Some 1.0 | Some x -> number doc x
                match encode, decode, order with
                | Some enc, Some dec, Some ord when enc.Length = inputs*2 && dec.Length = limits.Length && ord = floor ord && ord >= 0.0 && ord <= 3.0 ->
                    let data = try Some (Pdf.getStreamData doc obj) with _ -> None
                    data |> Option.bind (PdfSampled.create d limits size (int bits) (int ord) enc dec)
                    |> Option.map (fun eval -> wrap (limits.Length/2) eval [])
                | _ -> None
            | _ -> None
        | Some 4.0 ->
            match r with
            | Some limits ->
                let data = try Some (Pdf.getStreamData doc obj) with _ -> None
                data |> Option.bind (fun bytes -> PdfCalculator.parse bytes (limits.Length/2))
                |> Option.map (fun eval -> wrap (limits.Length/2) eval [])
            | None -> None
        | Some 2.0 when inputs = 1 && d.[0] < d.[1] ->
            let c0 = match field doc "C0" obj with None -> Some [|0.0|] | Some x -> numbers doc x
            let c1 = match field doc "C1" obj with None -> Some [|1.0|] | Some x -> numbers doc x
            match numberField doc "N" obj, c0, c1 with
            | Some n, Some c0, Some c1 when c0.Length > 0 && c0.Length = c1.Length && n > 0.0 && (d.[0] >= 0.0 || n = floor n) && (r |> Option.forall (fun a -> a.Length = c0.Length * 2)) ->
                let eval (x: float[]) (result: float[]) =
                    let t = Math.Pow(x.[0], n)
                    for i in 0 .. result.Length-1 do result.[i] <- c0.[i] + t * (c1.[i] - c0.[i])
                    true
                Some (wrap c0.Length eval [])
            | _ -> None
        | Some 3.0 when inputs = 1 && d.[0] < d.[1] ->
            match field doc "Functions" obj |> Option.map (Pdf.deref doc), arrayField doc "Bounds" obj, arrayField doc "Encode" obj with
            | Some (PdfArray fs), Some bounds, Some encode when not fs.IsEmpty && bounds.Length = fs.Length - 1 && encode.Length = fs.Length * 2 ->
                let edges = Array.concat [ [|d.[0]|]; bounds; [|d.[1]|] ]
                if not ([0 .. edges.Length - 2] |> List.forall (fun i -> edges.[i] < edges.[i+1])) then None else
                match fs |> List.map (parseFunction doc (depth + 1)) |> sequence with
                | None -> None
                | Some parsed ->
                    let funcs = List.toArray parsed
                    let eval (xs: float[]) (result: float[]) =
                        let x = xs.[0]
                        let mutable i = 0
                        while i < bounds.Length && x >= bounds.[i] do i <- i + 1
                        let t = (x - edges.[i]) / (edges.[i+1] - edges.[i])
                        let values = funcs.[i].Eval (encode.[2*i] + t * (encode.[2*i+1] - encode.[2*i]))
                        Array.Copy(values, result, result.Length)
                        true
                    let nested =
                        [ for i in 0 .. funcs.Length - 1 do
                            let a, b = encode.[2*i], encode.[2*i+1]
                            if a <> b then
                                for v in funcs.[i].Breaks do
                                    let t = (v - a) / (b - a)
                                    if t > 0.0 && t < 1.0 then yield edges.[i] + t * (edges.[i+1] - edges.[i]) ]
                    let outputs = funcs.[0].Outputs
                    if (funcs |> Array.exists (fun f -> f.Inputs <> 1 || f.Outputs <> outputs)) || (r |> Option.exists (fun a -> a.Length <> outputs * 2)) then None
                    else Some (wrap outputs eval (Array.toList bounds @ nested))
            | _ -> None
        | _ -> None
    | _ -> None

let tryParse (doc: PdfDocument) (obj: PdfObj) : Function option =
    match Pdf.deref doc obj with
    | PdfArray fs when not fs.IsEmpty ->
        match fs |> List.map (parseFunction doc 0) |> sequence with
        | Some (first :: _ as funcs) when funcs |> List.forall (fun f -> f.Inputs = first.Inputs && f.Outputs = 1) ->
            let funcs = List.toArray funcs
            let result = Array.zeroCreate<float> funcs.Length
            let single = Array.zeroCreate<float> 1
            let eval xs =
                for i in 0 .. funcs.Length-1 do result.[i] <- (funcs.[i].EvalN xs).[0]
                result
            Some { Eval = (fun x -> single.[0] <- x; eval single)
                   EvalN = eval; Inputs = first.Inputs; Outputs = funcs.Length
                   Breaks = funcs |> Array.toList |> List.collect (fun f -> f.Breaks) }
        | _ -> None
    | _ -> parseFunction doc 0 obj
