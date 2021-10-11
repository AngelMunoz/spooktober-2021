module Main


open Sutil
open Fable.Core.JsInterop
open Components

importSideEffects "./styles.css"

Decoration.register ()
// Start the app
App.view () |> Program.mountElement "spooktober"
