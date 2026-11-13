module Rbt.Pdf.Tests.JpxRegression

open System

// Original grayscale JP2 image on page 3 of real-jungle-1920.pdf (object 33).
// COD style 0x3e: reset, terminate every coding pass, vertically causal,
// predictable termination, and segmentation symbols; SOP and EPH are present.
// Pixel references are from Poppler pdfimages -f 3 -l 3 (every 17th column,
// every 19th row). FFmpeg independently decodes all 84,864 samples too.
let run (test: string -> (unit -> unit) -> unit) (check: string -> bool -> unit) =
    test "JPX terminated passes and vertically causal coding decode real image" (fun () ->
        let encoded = String.Concat [|
            "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"
        |]
        let expected = Array.concat [|
            [| 254; 250; 251; 255; 255; 255; 255; 255; 255; 250; 255; 253; 252; 255; 251; 255 |]
            [| 251; 255; 190; 243; 205; 213; 227; 217; 255; 213; 255; 234; 229; 248; 239; 255 |]
            [| 252; 21; 158; 90; 107; 156; 231; 61; 32; 254; 144; 203; 97; 148; 245; 44 |]
            [| 255; 252; 86; 237; 255; 22; 25; 254; 34; 248; 35; 106; 255; 246; 237; 142 |]
            [| 255; 255; 34; 41; 26; 41; 37; 38; 185; 30; 60; 36; 33; 38; 19; 253 |]
            [| 254; 251; 25; 25; 27; 161; 254; 254; 250; 254; 252; 159; 38; 26; 19; 222 |]
            [| 254; 144; 254; 35; 227; 249; 164; 254; 251; 255; 35; 242; 248; 35; 255; 253 |]
            [| 253; 29; 255; 254; 44; 45; 24; 18; 255; 29; 22; 21; 32; 255; 254; 80 |]
            [| 255; 67; 255; 252; 255; 150; 26; 28; 248; 28; 29; 171; 253; 255; 255; 18 |]
            [| 255; 255; 252; 252; 252; 251; 183; 30; 222; 56; 26; 250; 255; 253; 255; 252 |]
            [| 255; 255; 40; 255; 255; 60; 28; 30; 28; 27; 25; 30; 255; 255; 33; 255 |]
            [| 255; 255; 251; 247; 31; 249; 30; 27; 255; 27; 27; 254; 20; 254; 255; 255 |]
            [| 253; 255; 250; 70; 37; 255; 19; 250; 255; 255; 20; 223; 40; 52; 252; 255 |]
            [| 255; 250; 253; 255; 37; 37; 255; 253; 255; 253; 255; 32; 33; 255; 254; 255 |]
            [| 255; 255; 255; 253; 254; 48; 249; 255; 255; 254; 255; 27; 255; 255; 255; 255 |]
            [| 255; 255; 253; 252; 253; 253; 215; 237; 255; 255; 82; 251; 255; 254; 254; 254 |]
            [| 255; 255; 255; 253; 255; 255; 255; 249; 37; 251; 255; 252; 254; 254; 255; 254 |]
        |]
        let source = Convert.FromBase64String encoded
        match Rbt.Pdf.Render.JpxDecode.decode source with
        | Some (272, 312, 1, pixels) ->
            check "actual JPX decoded all grayscale samples" (pixels.Length = 272 * 312)
            let mutable correct = true
            let mutable i = 0
            for y = 0 to 312 - 1 do
                if y % 19 = 0 then
                    for x = 0 to 272 - 1 do
                        if x % 17 = 0 then
                            // Irreversible 9/7 transforms can round a value one unit
                            // differently from Poppler (also observed with FFmpeg).
                            correct <- correct && abs (int pixels[y * 272 + x] - int expected[i]) <= 1
                            i <- i + 1
            check "272 spread samples agree with independent Poppler pixels" (correct && i = expected.Length)
        | _ -> check "actual JPX image decoded with grayscale dimensions" false)
