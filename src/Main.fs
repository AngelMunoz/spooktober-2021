module Main


open Sutil
open Fable.Core.JsInterop
open Components

importSideEffects "./styles.css"

// Start the app
App.view () |> Program.mountElement "spooktober"
