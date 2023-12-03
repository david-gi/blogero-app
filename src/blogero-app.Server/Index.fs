module blogero_app.Server.Index

open Bolero
open Bolero.Html
open Bolero.Server.Html
open blogero_app

let page = doctypeHtml {
    head { 
        meta { attr.charset "UTF-8" }
        meta { attr.name "viewport"; attr.content "width=device-width, initial-scale=1.0" }
        title { "Blogero: F# + Wasm + Immutable Blogging" }
        ``base`` { attr.href "/" }
        script { attr.src "css/tailwind.js" } 
        link { attr.rel "stylesheet"; attr.href "css/index.css" }
        link { attr.rel "icon"; attr.href "favicon.png" }
    }
    body {
        div { attr.id "main"; comp<Client.Main.MyApp> }
        boleroScript
    }
}
