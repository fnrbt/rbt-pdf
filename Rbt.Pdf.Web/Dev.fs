module Rbt.Pdf.Web.Dev

// Dev shell entry point. Only loaded by index.html during development.
// NOT included in the npm package build (build:fable targets App.fs as the library entry).

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Rbt.Pdf.Web.WebApi
open Rbt.Pdf.Web.App

[<Emit("fetch($0).then(function(r){ if(!r.ok) throw new Error('fetch '+$0+': '+r.status); return r.arrayBuffer(); }).then(function(b){ return new Uint8Array(b); })")>]
let private fetchBytes (url: string) : JS.Promise<byte[]> = jsNative

[<Emit("$0.style[$1] = $2")>]
let private setS (el: obj) (prop: string) (v: string) : unit = jsNative

let private devEntry () =
    let root = Browser.Dom.document.getElementById "pdf-root"
    if not (isNull root) then
        let urlParams : obj = emitJsExpr () "new URLSearchParams(window.location.search)"
        let pdfUrl : string option =
            let v : string = emitJsExpr urlParams "$0.get('url')"
            if isNull v then None else Some v

        match pdfUrl with
        | Some url ->
            fetchBytes url
            |> promiseBind (fun bytes ->
                openPdf (root :?> HTMLElement, bytes, "./assets")
            )
            |> promiseMap (fun _viewer ->
                emitJsExpr () "console.log('PDF opened from URL')"
            )
            |> ignore
        | None ->
            let input : HTMLInputElement = downcast Browser.Dom.document.createElement "input"
            input.``type`` <- "file"
            input.accept <- ".pdf,application/pdf"
            emitJsExpr (input, fun () ->
                let files : obj = emitJsExpr input "$0.files"
                let fileCount : int = emitJsExpr files "$0.length"
                if fileCount > 0 then
                    let file : obj = emitJsExpr files "$0[0]"
                    let p : JS.Promise<obj> = emitJsExpr file "$0.arrayBuffer()"
                    p
                    |> promiseBind (fun buf ->
                        let bytes : byte[] = emitJsExpr buf "new Uint8Array($0)"
                        openPdf (root :?> HTMLElement, bytes, "./assets")
                    )
                    |> promiseMap (fun _viewer ->
                        emitJsExpr () "console.log('PDF opened from file')"
                    )
                    |> ignore
            ) "$0.addEventListener('change', $1)"
            let label = Browser.Dom.document.createElement "div"
            setS label "padding" "24px"
            setS label "fontFamily" "system-ui, sans-serif"
            label.innerHTML <- "<p>Select a PDF file to view:</p>"
            label.appendChild(input) |> ignore
            root.appendChild(label) |> ignore

devEntry ()
