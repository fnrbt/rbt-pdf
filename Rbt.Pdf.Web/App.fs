module Rbt.Pdf.Web.App

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Rbt.Pdf
open Rbt.Pdf.Render.Render
open Rbt.Renderer
open Rbt.Pdf.Web.WebApi

// ── Helpers ──────────────────────────────────────────────

[<Emit("fetch($0).then(function(r){ if(!r.ok) throw new Error('fetch '+$0+': '+r.status); return r.arrayBuffer(); }).then(function(b){ return new Uint8Array(b); })")>]
let private fetchBytes (url: string) : JS.Promise<byte[]> = jsNative

[<Emit("$0.style[$1] = $2")>]
let private setS (el: obj) (prop: string) (v: string) : unit = jsNative

// ── JS-friendly viewer interface ─────────────────────────

type PdfViewer =
    abstract setZoom: float -> unit
    abstract zoom: float
    abstract pageCount: int
    abstract scrollToPage: int -> unit
    abstract dispose: unit -> unit

// ── Asset loading (memoized) ─────────────────────────────

type private Assets = { Font: Text.FontData; Opts: RenderOptions }

let private assetCache = System.Collections.Generic.Dictionary<string, JS.Promise<Assets>>()

let private loadAssets (assetsBase: string) : JS.Promise<Assets> =
    let key = assetsBase.TrimEnd('/')
    match assetCache.TryGetValue(key) with
    | true, p -> p
    | _ ->
        let p =
            let ucdUrl = key + "/ucd.dat"
            let fontUrl = key + "/NotoSans-Regular.ttf"
            promiseAll [| fetchBytes ucdUrl; fetchBytes fontUrl |]
            |> promiseMap (fun results ->
                Rbt.Unicode.Ucd.loadData results.[0]
                let font = Text.loadFont results.[1]
                { Font = font; Opts = { FallbackFont = font; DecodeJpeg = ImageSource.decodeJpeg } })
        assetCache.[key] <- p
        p

// ── Public API ───────────────────────────────────────────

/// Open a PDF inside the given container element.
/// `bytes`      — raw PDF file contents (Uint8Array from JS).
/// `assetsBase` — URL prefix for ucd.dat and NotoSans-Regular.ttf (e.g. "./assets").
/// Returns a Promise that resolves to a PdfViewer handle.
let openPdf (container: HTMLElement, bytes: byte[], assetsBase: string) : JS.Promise<PdfViewer> =
    loadAssets assetsBase
    |> promiseMap (fun assets ->
        let doc = Pdf.openData bytes
        let view = PageView.PageView(container, doc, assets.Opts)
        { new PdfViewer with
            member _.setZoom(z) = view.SetZoom(z)
            member _.zoom = view.Zoom
            member _.pageCount = view.PageCount
            member _.scrollToPage(n) = view.ScrollToPage(n)
            member _.dispose() =
                view.Dispose()
                GlyphCache.clear () }
    )
