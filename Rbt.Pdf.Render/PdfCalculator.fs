module Rbt.Pdf.Render.PdfCalculator

open System
open System.Globalization

// Limits apply independently to each parsed program and each evaluation.
let private maxBytes = 1048576
let private maxTokens = 65536
let private maxNesting = 64
let private maxStack = 1024
let private maxSteps = 100000

exception private InvalidCalculator

[<Struct>]
type private Value =
    | Integer of integer: int
    | Real of real: float
    | Boolean of boolean: bool
    | Procedure of procedure: Instruction[]
and private Instruction =
    | Literal of Value
    | Operator of string

let private finite x = not (Double.IsNaN x || Double.IsInfinity x)
let private fail () = raise InvalidCalculator

let private operators =
    set [ "abs"; "add"; "atan"; "ceiling"; "cos"; "cvi"; "cvr"; "div"; "exp"; "floor";
          "idiv"; "ln"; "log"; "mod"; "mul"; "neg"; "round"; "sin"; "sqrt"; "sub"; "truncate";
          "and"; "bitshift"; "eq"; "ge"; "gt"; "le"; "lt"; "ne"; "not"; "or"; "xor";
          "if"; "ifelse"; "copy"; "dup"; "exch"; "index"; "pop"; "roll" ]

let private parseProgram (bytes: byte[]) =
    if isNull bytes || bytes.Length > maxBytes then fail ()
    let mutable pos = 0
    let mutable tokens = 0
    let whitespace c = c = 0uy || c = 9uy || c = 10uy || c = 12uy || c = 13uy || c = 32uy
    let skip () =
        let mutable again = true
        while again do
            while pos < bytes.Length && whitespace bytes.[pos] do pos <- pos + 1
            if pos < bytes.Length && bytes.[pos] = 37uy then
                while pos < bytes.Length && bytes.[pos] <> 10uy && bytes.[pos] <> 13uy do pos <- pos + 1
            else again <- false
    let count () =
        tokens <- tokens + 1
        if tokens > maxTokens then fail ()
    let number (s: string) =
        let mutable digits = 0
        let mutable dots = 0
        for i = 0 to s.Length - 1 do
            let c = s.[i]
            if c >= '0' && c <= '9' then digits <- digits + 1
            elif c = '.' then dots <- dots + 1
            elif (c = '+' || c = '-') && i = 0 then ()
            else fail ()
        if digits = 0 || dots > 1 then fail ()
        let mutable n = 0
        if dots = 0 && Int32.TryParse(s, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture, &n) then Integer n
        else
            let mutable x = 0.0
            if not (Double.TryParse(s, NumberStyles.AllowLeadingSign ||| NumberStyles.AllowDecimalPoint, CultureInfo.InvariantCulture, &x)) || not (finite x) then fail ()
            Real x
    let rec procedure depth =
        if depth > maxNesting then fail ()
        let items = ResizeArray<Instruction>()
        let mutable closed = false
        while not closed do
            skip ()
            if pos >= bytes.Length then fail ()
            match bytes.[pos] with
            | 125uy -> pos <- pos + 1; count (); closed <- true
            | 123uy ->
                pos <- pos + 1
                count ()
                items.Add(Literal (Procedure (procedure (depth + 1))))
            | _ ->
                let start = pos
                while pos < bytes.Length && not (whitespace bytes.[pos]) && bytes.[pos] <> 123uy && bytes.[pos] <> 125uy && bytes.[pos] <> 37uy do
                    if bytes.[pos] > 127uy then fail ()
                    pos <- pos + 1
                if pos = start then fail ()
                count ()
                let s = System.Text.Encoding.UTF8.GetString(bytes, start, pos - start)
                match s with
                | "true" -> items.Add(Literal (Boolean true))
                | "false" -> items.Add(Literal (Boolean false))
                | _ when Set.contains s operators -> items.Add(Operator s)
                | _ -> items.Add(Literal (number s))
        items.ToArray()
    skip ()
    if pos >= bytes.Length || bytes.[pos] <> 123uy then fail ()
    pos <- pos + 1
    count ()
    let program = procedure 1
    skip ()
    if pos <> bytes.Length then fail ()
    program

type private Machine(program: Instruction[], expectedOutputs: int) =
    let stack = Array.create maxStack (Boolean false)
    let mutable count = 0
    let push v =
        if count >= maxStack then fail ()
        stack.[count] <- v
        count <- count + 1
    let pop () =
        if count = 0 then fail ()
        count <- count - 1
        stack.[count]
    let intValue = function Integer x -> x | _ -> fail ()
    let num = function Integer x -> float x | Real x -> x | _ -> fail ()
    let real x = if finite x then Real x else fail ()
    let integerOrReal x =
        if not (finite x) then fail ()
        elif x >= float Int32.MinValue && x <= float Int32.MaxValue then Integer (int x)
        else Real x
    let binaryNumeric op =
        let b = pop ()
        let a = pop ()
        let x = num a
        let y = num b
        let result = match op with "add" -> x + y | "sub" -> x - y | _ -> x * y
        match a, b with
        | Integer _, Integer _ -> push (integerOrReal result)
        | _ -> push (real result)
    let binaryReal op =
        let b = num (pop ())
        let a = num (pop ())
        let result =
            match op with
            | "div" -> if b = 0.0 then fail () else a / b
            | "atan" ->
                if a = 0.0 && b = 0.0 then fail ()
                let angle = Math.Atan2(a, b) * 180.0 / Math.PI
                if angle < 0.0 then angle + 360.0 else angle
            | _ -> Math.Pow(a, b)
        push (real result)
    let unaryNumeric op =
        let a = pop ()
        let x = num a
        let result =
            match op with
            | "neg" -> -x
            | "abs" -> abs x
            | "ceiling" -> Math.Ceiling x
            | "floor" -> Math.Floor x
            | "round" ->
                let lower = Math.Floor x
                if x - lower >= 0.5 then lower + 1.0 else lower
            | _ -> Math.Truncate x
        match a with Integer _ -> push (integerOrReal result) | _ -> push (real result)
    let unaryReal op =
        let x = num (pop ())
        let result =
            match op with
            | "sqrt" -> if x < 0.0 then fail () else Math.Sqrt x
            | "sin" -> Math.Sin(x * Math.PI / 180.0)
            | "cos" -> Math.Cos(x * Math.PI / 180.0)
            | "ln" -> if x <= 0.0 then fail () else Math.Log x
            | _ -> if x <= 0.0 then fail () else Math.Log10 x
        push (real result)
    let compareNumbers op =
        let b = num (pop ())
        let a = num (pop ())
        push (Boolean (match op with "gt" -> a > b | "ge" -> a >= b | "lt" -> a < b | _ -> a <= b))
    let reverse first last =
        let mutable a = first
        let mutable b = last
        while a < b do
            let v = stack.[a]
            stack.[a] <- stack.[b]
            stack.[b] <- v
            a <- a + 1
            b <- b - 1
    let mutable steps = 0
    let rec run depth (code: Instruction[]) =
        if depth > maxNesting then fail ()
        for instruction in code do
            steps <- steps + 1
            if steps > maxSteps then fail ()
            match instruction with
            | Literal v -> push v
            | Operator op ->
                match op with
                | "add" -> binaryNumeric op
                | "sub" -> binaryNumeric op
                | "mul" -> binaryNumeric op
                | "div" -> binaryReal op
                | "idiv" | "mod" ->
                    let b = intValue (pop ())
                    let a = intValue (pop ())
                    if b = 0 then fail ()
                    if a = Int32.MinValue && b = -1 then
                        if op = "idiv" then fail () else push (Integer 0)
                    else push (Integer (if op = "idiv" then a / b else a % b))
                | "neg" -> unaryNumeric op
                | "abs" -> unaryNumeric op
                | "ceiling" -> unaryNumeric op
                | "floor" -> unaryNumeric op
                | "round" -> unaryNumeric op
                | "truncate" -> unaryNumeric op
                | "sqrt" -> unaryReal op
                | "sin" -> unaryReal op
                | "cos" -> unaryReal op
                | "atan" -> binaryReal op
                | "exp" -> binaryReal op
                | "ln" -> unaryReal op
                | "log" -> unaryReal op
                | "cvi" ->
                    let x = Math.Truncate(num (pop ()))
                    if x < float Int32.MinValue || x > float Int32.MaxValue then fail ()
                    push (Integer (int x))
                | "cvr" -> push (real (num (pop ())))
                | "eq" | "ne" ->
                    let b = pop ()
                    let a = pop ()
                    let equal =
                        match a, b with
                        | Boolean x, Boolean y -> x = y
                        | Integer _, Integer _ | Integer _, Real _ | Real _, Integer _ | Real _, Real _ -> num a = num b
                        | Procedure _, _ | _, Procedure _ -> fail ()
                        | _ -> false
                    push (Boolean (if op = "eq" then equal else not equal))
                | "gt" -> compareNumbers op
                | "ge" -> compareNumbers op
                | "lt" -> compareNumbers op
                | "le" -> compareNumbers op
                | "and" | "or" | "xor" ->
                    let b = pop ()
                    let a = pop ()
                    match a, b with
                    | Boolean x, Boolean y ->
                        push (Boolean (match op with "and" -> x && y | "or" -> x || y | _ -> x <> y))
                    | Integer x, Integer y ->
                        push (Integer (match op with "and" -> x &&& y | "or" -> x ||| y | _ -> x ^^^ y))
                    | _ -> fail ()
                | "not" ->
                    match pop () with
                    | Boolean x -> push (Boolean (not x))
                    | Integer x -> push (Integer (~~~x))
                    | _ -> fail ()
                | "bitshift" ->
                    let shift = intValue (pop ())
                    let x = intValue (pop ())
                    let result =
                        if shift >= 32 || shift <= -32 then 0
                        elif shift >= 0 then int (uint32 x <<< shift)
                        else int (uint32 x >>> -shift)
                    push (Integer result)
                | "if" ->
                    let p = pop ()
                    let c = pop ()
                    match c, p with
                    | Boolean b, Procedure body -> if b then run (depth + 1) body
                    | _ -> fail ()
                | "ifelse" ->
                    let no = pop ()
                    let yes = pop ()
                    let c = pop ()
                    match c, yes, no with
                    | Boolean b, Procedure a, Procedure d -> run (depth + 1) (if b then a else d)
                    | _ -> fail ()
                | "pop" -> pop () |> ignore
                | "dup" -> let a = pop () in push a; push a
                | "exch" -> let b = pop () in let a = pop () in push b; push a
                | "copy" ->
                    let n = intValue (pop ())
                    if n < 0 || n > count || n > maxStack - count then fail ()
                    let start = count - n
                    for i = 0 to n - 1 do push stack.[start + i]
                | "index" ->
                    let n = intValue (pop ())
                    if n < 0 || n >= count then fail ()
                    push stack.[count - 1 - n]
                | "roll" ->
                    let j = intValue (pop ())
                    let n = intValue (pop ())
                    if n < 0 || n > count then fail ()
                    if n > 1 then
                        let shift = ((j % n) + n) % n
                        if shift <> 0 then
                            let start = count - n
                            reverse start (count - 1)
                            reverse start (start + shift - 1)
                            reverse (start + shift) (count - 1)
                | _ -> fail ()

    member _.Evaluate(inputs: float[], output: float[]) =
        try
            if isNull inputs || inputs.Length > maxStack || isNull output || output.Length <> expectedOutputs then fail ()
            count <- 0
            steps <- 0
            for x in inputs do push (real x)
            run 1 program
            if count <> expectedOutputs then fail ()
            for i = 0 to count - 1 do output.[i] <- num stack.[i]
            true
        with InvalidCalculator -> false

/// Parse a PDF type 4 calculator program once, returning a reusable evaluator.
/// The caller owns both arrays and must synchronize access to the evaluator.
let parse (bytes: byte[]) (expectedOutputs: int) : (float[] -> float[] -> bool) option =
    try
        if expectedOutputs < 0 || expectedOutputs > maxStack then fail ()
        let machine = Machine(parseProgram bytes, expectedOutputs)
        Some (fun inputs output -> machine.Evaluate(inputs, output))
    with InvalidCalculator -> None
