module Rbt.Pdf.Render.BitmapPaint

open Rbt.Renderer

/// PDF images occupy a y-up unit square; their stored rows run top-down.
let command (pixels: uint32[]) width height interpolate =
    let flip = { M11 = 1.0; M12 = 0.0; M21 = 0.0; M22 = -1.0; DX = 0.0; DY = 1.0 }
    Group(flip, 1.0, [DrawImageReduced(Framebuffer.ofArray pixels width height, rect 0.0 0.0 (float width) (float height), rect 0.0 0.0 1.0 1.0, interpolate)])

