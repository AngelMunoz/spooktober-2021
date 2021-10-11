module Components.Stage

open Sutil
open Browser.Types


let Stage (props: IStore<_>) (host: Node) = Html.article []

let register () =
    WebComponent.Register("sp-stage", Stage, ())
