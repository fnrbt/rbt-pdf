module Rbt.Pdf.Render.PageGeometry

open System
open Rbt.Pdf
open Rbt.Renderer

/// Visible page bounds in PDF coordinates and the unscaled page-to-screen map.
type Viewport = {
    Bounds: float * float * float * float
    Width: float
    Height: float
    Transform: Matrix3x2
}

let private number doc value =
    match Pdf.deref doc value with
    | PdfInt n -> Some (float n)
    | PdfReal n when not (Double.IsNaN n || Double.IsInfinity n) -> Some n
    | _ -> None

let private box doc value =
    match Pdf.deref doc value with
    | PdfArray [a; b; c; d] ->
        match number doc a, number doc b, number doc c, number doc d with
        | Some x0, Some y0, Some x1, Some y1 when x0 <> x1 && y0 <> y1 ->
            Some (min x0 x1, min y0 y1, max x0 x1, max y0 y1)
        | _ -> None
    | _ -> None

/// MediaBox, CropBox and Rotate are inherited; CropBox is limited to MediaBox.
let tryGet (doc: PdfDocument) (page: PdfObj) =
    match Pdf.getPageInheritable doc page "MediaBox" |> Option.bind (box doc) with
    | None -> None
    | Some (mx0, my0, mx1, my1) ->
        let bounds =
            match Pdf.getPageInheritable doc page "CropBox" |> Option.bind (box doc) with
            | Some (cx0, cy0, cx1, cy1) ->
                max mx0 cx0, max my0 cy0, min mx1 cx1, min my1 cy1
            | None -> mx0, my0, mx1, my1
        let x0, y0, x1, y1 = bounds
        if x1 <= x0 || y1 <= y0 then None
        else
            let rotation =
                match Pdf.getPageInheritable doc page "Rotate" |> Option.map (Pdf.deref doc) with
                | Some (PdfInt r) -> int ((r % 360L + 360L) % 360L)
                | _ -> 0
            let width, height = x1 - x0, y1 - y0
            let transform, w, h =
                match rotation with
                | 90 ->
                    { M11 = 0.0; M12 = 1.0; M21 = 1.0; M22 = 0.0; DX = -y0; DY = -x0 }, height, width
                | 180 ->
                    { M11 = -1.0; M12 = 0.0; M21 = 0.0; M22 = 1.0; DX = x1; DY = -y0 }, width, height
                | 270 ->
                    { M11 = 0.0; M12 = -1.0; M21 = -1.0; M22 = 0.0; DX = y1; DY = x1 }, height, width
                | _ ->
                    { M11 = 1.0; M12 = 0.0; M21 = 0.0; M22 = -1.0; DX = -x0; DY = y1 }, width, height
            Some { Bounds = bounds; Width = w; Height = h; Transform = transform }
