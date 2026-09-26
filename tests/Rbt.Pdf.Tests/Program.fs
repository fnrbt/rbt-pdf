/// Parser tolerance and page-copy tests on small hand-built PDFs.
/// Run: dotnet run --project tests/Rbt.Pdf.Tests
module Rbt.Pdf.Tests.Program

open System
open System.Text
open Rbt.Pdf
open Rbt.Pdf.Types

let latin1 (s: string) = Encoding.Latin1.GetBytes s
let text (b: byte[]) = Encoding.Latin1.GetString b

/// A minimal PDF from (object number, body) pairs: header, objects, an xref
/// table, trailer and startxref, with correct offsets unless `layout` says
/// otherwise.
type Layout =
    { Prefix: string                 // bytes before %PDF (junk)
      OffsetsIncludePrefix: bool     // xref offsets counted from file start
      TrailerOnXRefLine: bool        // "trailer <<...>>" on one line
      OmitXRef: bool                 // no xref, trailer or startxref at all
      GarbleOffsets: bool            // every xref offset wrong
      StartXRefBogus: bool }         // startxref points into an object

let normal =
    { Prefix = ""; OffsetsIncludePrefix = false; TrailerOnXRefLine = false
      OmitXRef = false; GarbleOffsets = false; StartXRefBogus = false }

let build (layout: Layout) (objects: (int * string) list) (rootNum: int) =
    let sb = StringBuilder()
    sb.Append("%PDF-1.4\n%\xe2\xe3\xcf\xd3\n") |> ignore
    let offsets = Collections.Generic.Dictionary<int, int>()
    for num, body in objects do
        offsets.[num] <- sb.Length
        sb.Append($"{num} 0 obj\n{body}\nendobj\n") |> ignore
    let size = (objects |> List.map fst |> List.max) + 1
    if not layout.OmitXRef then
        let xrefPos = sb.Length
        sb.Append($"xref\n0 {size}\n0000000000 65535 f\r\n") |> ignore
        for n in 1 .. size - 1 do
            match offsets.TryGetValue n with
            | true, off ->
                let off =
                    if layout.GarbleOffsets then off + 3
                    elif layout.OffsetsIncludePrefix then off + layout.Prefix.Length
                    else off
                sb.Append(off.ToString("D10")).Append(" 00000 n\r\n") |> ignore
            | _ -> sb.Append("0000000000 00000 f\r\n") |> ignore
        let trailer = $"<</Size {size} /Root {rootNum} 0 R>>"
        if layout.TrailerOnXRefLine then sb.Append($"trailer {trailer}\n\n") |> ignore
        else sb.Append($"trailer\n{trailer}\n") |> ignore
        let startxref =
            if layout.StartXRefBogus then offsets.[rootNum] + 4
            elif layout.OffsetsIncludePrefix then xrefPos + layout.Prefix.Length
            else xrefPos
        sb.Append($"startxref\n{startxref}\n%%%%EOF\n") |> ignore
    Array.append (latin1 layout.Prefix) (latin1 (sb.ToString()))

/// One page with inline content "BT (Hello) Tj ET".
let onePage =
    [ 1, "<</Type /Catalog /Pages 2 0 R>>"
      2, "<</Type /Pages /Kids [3 0 R] /Count 1>>"
      3, "<</Type /Page /Parent 2 0 R /MediaBox [0 0 300 400] /Contents 4 0 R>>"
      4, "<</Length 18>>\nstream\nBT (Hello) Tj ET\n\nendstream" ]

let mutable failures = 0
let check name cond =
    if cond then printfn "ok   %s" name
    else
        failures <- failures + 1
        printfn "FAIL %s" name

let test name (f: unit -> unit) =
    try f ()
    with ex ->
        failures <- failures + 1
        printfn "FAIL %s: %s" name (ex.ToString())

let pageContent (doc: PdfDocument) (n: int) =
    let page = Pdf.getPageRefs doc |> List.item (n - 1)
    PageOps.getPageContents doc page |> text

let assertHello name (bytes: byte[]) =
    let doc = Pdf.openData bytes
    check $"{name}: one page" (Pdf.getPageRefs doc |> List.length = 1)
    check $"{name}: content" ((pageContent doc 1).Contains "BT (Hello) Tj ET")
    // And it survives a round trip through merge.
    let merged = Operations.merge [ doc ] |> Operations.toBytes |> Pdf.openData
    check $"{name}: merged content" ((pageContent merged 1).Contains "BT (Hello) Tj ET")

[<EntryPoint>]
let main _ =
    test "baseline" (fun () -> assertHello "baseline" (build normal onePage 1))

    test "junk prefix" (fun () ->
        // NV Energy bills: 8 bytes before %PDF, offsets relative to the header.
        assertHello "junk prefix" (build { normal with Prefix = "[BEGIN]\n" } onePage 1))

    test "junk prefix, file-relative offsets" (fun () ->
        assertHello "junk prefix, file-relative offsets"
            (build { normal with Prefix = "[BEGIN]\n"; OffsetsIncludePrefix = true } onePage 1))

    test "header beyond 1 KB" (fun () ->
        let bytes = build { normal with Prefix = String('x', 1100) } onePage 1
        let refused = try Pdf.openData bytes |> ignore; false with ex -> ex.Message.Contains "Not a PDF"
        check "header beyond 1 KB is refused" refused)

    test "trailer on the xref line" (fun () ->
        // Henderson water bills: "trailer <</Size 57 /Root 56 0 R>>" on one line.
        assertHello "trailer on the xref line" (build { normal with TrailerOnXRefLine = true } onePage 1))

    test "no xref" (fun () ->
        assertHello "no xref (reconstructed)" (build { normal with OmitXRef = true } onePage 1))

    test "garbled offsets" (fun () ->
        assertHello "garbled offsets (repaired)" (build { normal with GarbleOffsets = true } onePage 1))

    test "bogus startxref" (fun () ->
        assertHello "bogus startxref (reconstructed)" (build { normal with StartXRefBogus = true } onePage 1))

    test "garbled xref section" (fun () ->
        let bytes = build normal onePage 1 |> text
        let broken = bytes.Replace("0000000000 65535 f\r\n", "0000000000 65535 f\r\nthis is not an xref entry\r\n")
        assertHello "garbled xref section (reconstructed)" (latin1 broken))

    test "inherited page attributes" (fun () ->
        // MediaBox, Resources and Rotate live on the Pages node, not the page.
        let objects =
            [ 1, "<</Type /Catalog /Pages 2 0 R>>"
              2, "<</Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 200 100] /Rotate 90 /Resources <</Font <</F1 5 0 R>>>>>>"
              3, "<</Type /Page /Parent 2 0 R /Contents 4 0 R>>"
              4, "<</Length 18>>\nstream\nBT (Hello) Tj ET\nendstream"
              5, "<</Type /Font /Subtype /Type1 /BaseFont /Helvetica>>" ]
        let doc = Pdf.openData (build normal objects 1)
        let check' label (copy: PdfDocument) =
            let copy = copy |> Operations.toBytes |> Pdf.openData
            let page = Pdf.getPageRefs copy |> List.head
            let d = match Pdf.deref copy page with PdfDict d -> d | _ -> Map.empty
            check $"{label}: MediaBox on the page" (Map.tryFind "MediaBox" d = Some (PdfArray [ PdfInt 0L; PdfInt 0L; PdfInt 200L; PdfInt 100L ]))
            check $"{label}: Rotate on the page" (Map.tryFind "Rotate" d = Some (PdfInt 90L))
            let font =
                Map.tryFind "Resources" d
                |> Option.bind (fun r -> Pdf.dictGet copy "Font" r)
                |> Option.bind (fun f -> Pdf.dictGet copy "F1" f)
                |> Option.map (Pdf.deref copy)
            check $"{label}: Resources font copied" (match font with Some (PdfDict f) -> Map.tryFind "BaseFont" f = Some (PdfName "Helvetica") | _ -> false)
        check' "merge" (Operations.merge [ doc ])
        check' "extractPages" (Operations.extractPages doc [ 1 ]))

    test "annotation cycle and long chains" (fun () ->
        // An annotation's /P points back at its page, and a 20,000-link chain
        // of objects hangs off the page: neither may loop or overflow the stack.
        let chain = 20_000
        let objects =
            [ yield 1, "<</Type /Catalog /Pages 2 0 R>>"
              yield 2, "<</Type /Pages /Kids [3 0 R 6 0 R] /Count 2>>"
              yield 3, "<</Type /Page /Parent 2 0 R /MediaBox [0 0 300 400] /Contents 4 0 R /Annots [5 0 R] /Chain 10 0 R>>"
              yield 4, "<</Length 18>>\nstream\nBT (Hello) Tj ET\nendstream"
              yield 5, "<</Type /Annot /Subtype /Link /Rect [0 0 10 10] /P 3 0 R /Dest [6 0 R /Fit]>>"
              yield 6, "<</Type /Page /Parent 2 0 R /MediaBox [0 0 300 400]>>"
              for i in 0 .. chain - 1 do
                  let next = if i = chain - 1 then "null" else $"{11 + i} 0 R"
                  yield 10 + i, $"<</Next {next} /Up 3 0 R>>" ]
        let doc = Pdf.openData (build normal objects 1)
        // Page 1 only: the link to page 2 must not drag page 2 in.
        let copy = Operations.extractPages doc [ 1 ] |> Operations.toBytes |> Pdf.openData
        let pages = Pdf.getPageRefs copy
        check "cycle: one page" (pages.Length = 1)
        let page = pages.Head
        let annot = Pdf.dictGet copy "Annots" page |> Option.map (Pdf.deref copy)
        match annot with
        | Some (PdfArray [ a ]) ->
            check "cycle: annotation /P is the copied page" (Pdf.dictGet copy "P" a = Some page)
            check "cycle: link to an uncopied page is null" (Pdf.dictGet copy "Dest" a = Some (PdfArray [ PdfNull; PdfName "Fit" ]))
        | _ -> check "cycle: annotation copied" false
        let rec chainLength n (node: PdfObj option) =
            match node |> Option.map (Pdf.deref copy) with
            | Some (PdfDict d) -> chainLength (n + 1) (Map.tryFind "Next" d)
            | _ -> n
        check "cycle: whole chain copied" (chainLength 0 (Pdf.dictGet copy "Chain" page) = chain))

    test "stream trailing newlines" (fun () ->
        // Stream data that itself ends in CR/LF bytes, with an indirect /Length
        // (the parser must not trim the data's own newlines) and with a direct one.
        let data = "x\x9c\n\r\n"  // arbitrary binary ending in LF CR LF
        let objects =
            [ 1, "<</Type /Catalog /Pages 2 0 R>>"
              2, "<</Type /Pages /Kids [3 0 R] /Count 1>>"
              3, "<</Type /Page /Parent 2 0 R /MediaBox [0 0 300 400] /Resources <</XObject <</A 4 0 R /B 6 0 R>>>>>>"
              4, $"<</Length 5 0 R>>\nstream\n{data}\nendstream"
              5, string data.Length
              6, $"<</Length {data.Length}>>\nstream\n{data}\nendstream" ]
        let raw (doc: PdfDocument) name =
            let page = Pdf.getPageRefs doc |> List.head
            Pdf.getPageInheritable doc page "Resources"
            |> Option.bind (Pdf.dictGet doc "XObject")
            |> Option.bind (Pdf.dictGet doc name)
            |> Option.map (Pdf.deref doc)
            |> function Some (PdfStream (_, d)) -> text d | _ -> "<none>"
        let doc = Pdf.openData (build normal objects 1)
        check "stream: indirect /Length keeps trailing newlines" (raw doc "A" = data)
        check "stream: direct /Length keeps trailing newlines" (raw doc "B" = data)
        let copy = Operations.merge [ doc ] |> Operations.toBytes |> Pdf.openData
        check "stream: indirect /Length survives merge" (raw copy "A" = data)
        check "stream: direct /Length survives merge" (raw copy "B" = data))

    test "stream with a wrong /Length" (fun () ->
        let objects =
            [ 1, "<</Type /Catalog /Pages 2 0 R>>"
              2, "<</Type /Pages /Kids [3 0 R] /Count 1>>"
              3, "<</Type /Page /Parent 2 0 R /MediaBox [0 0 300 400] /Contents 4 0 R>>"
              4, "<</Length 99>>\nstream\nBT (Hello) Tj ET\nendstream" ]
        assertHello "wrong /Length" (build normal objects 1))

    if failures > 0 then
        printfn "%d failure(s)" failures
        1
    else
        printfn "all passed"
        0
