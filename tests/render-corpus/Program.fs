module Rbt.Pdf.Corpus.Program

open System
open System.IO
open System.Text
open Rbt.Pdf
open Rbt.Renderer
open Rbt.Pdf.Render.Render

// One page per process: crashes/timeouts and global font caches cannot contaminate
// other documents. The corpus driver bounds execution time.
[<EntryPoint>]
let main args =
    if args.Length <> 4 then
        eprintfn "Usage: RenderCorpus input.pdf page max-side output.ppm"
        2
    else
        try
            let page = Int32.Parse args.[1]
            let maxSide = Int32.Parse args.[2]
            if page < 1 || maxSide < 1 then invalidArg "args" "page and max-side must be positive"
            let doc = Pdf.openData (File.ReadAllBytes args.[0])
            let viewport =
                Pdf.getPage doc page
                |> Option.bind (Rbt.Pdf.Render.PageGeometry.tryGet doc)
                |> Option.defaultWith (fun () -> failwithf "Page %d has no valid viewport" page)
            let scale = float maxSide / max viewport.Width viewport.Height
            match renderPage (RenderOptions.native ()) doc page scale with
            | None -> failwithf "Page %d could not be rendered" page
            | Some (commands, width, height, _) ->
                if not (Double.IsFinite width && Double.IsFinite height) || width <= 0.0 || height <= 0.0 then
                    failwith "Invalid page dimensions"
                let w = max 1 (int (ceil width))
                let h = max 1 (int (ceil height))
                let backend = Raster.create ()
                backend.Init w h
                backend.Submit (Clear Color.white :: commands)
                let fb = backend.Finish ()
                use output = File.Create args.[3]
                output.Write(Encoding.ASCII.GetBytes $"P6\n{w} {h}\n255\n")
                let row = Array.zeroCreate<byte> (w * 3)
                let pixels = fb.Px.Span
                for y in 0 .. h - 1 do
                    for x in 0 .. w - 1 do
                        let pixel = pixels.[y * w + x]
                        row.[x * 3] <- Pixel.r pixel
                        row.[x * 3 + 1] <- Pixel.g pixel
                        row.[x * 3 + 2] <- Pixel.b pixel
                    output.Write row
                printfn "%dx%d" w h
                0
        with ex ->
            eprintfn "%s" (ex.ToString())
            1
