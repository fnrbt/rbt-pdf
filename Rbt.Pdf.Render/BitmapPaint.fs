module Rbt.Pdf.Render.BitmapPaint

open Rbt.Renderer

let private rectangle x0 y0 x1 y1 =
    [MoveTo(vec2 x0 y0); LineTo(vec2 x1 y0); LineTo(vec2 x1 y1); LineTo(vec2 x0 y1); Close]


/// PDF images occupy a y-up unit square; their stored rows run top-down.
let command (pixels: uint32[]) width height interpolate =
    if not interpolate then
        let flip = { M11 = 1.0; M12 = 0.0; M21 = 0.0; M22 = -1.0; DX = 0.0; DY = 1.0 }
        Group(flip, 1.0, [DrawImage(Framebuffer.ofArray pixels width height, rect 0.0 0.0 (float width) (float height), rect 0.0 0.0 1.0 1.0, false)])
    else
        let image = Framebuffer.ofArray pixels width height
        let sx, sy = 1.0 / float width, 1.0 / float height
        let placement = { M11 = sx; M12 = 0.0; M21 = 0.0; M22 = -sy; DX = sx * 0.5; DY = 1.0 - sy * 0.5 }
        FillPath(rectangle 0.0 0.0 1.0 1.0, ImagePaint(image, placement), NonZero)

