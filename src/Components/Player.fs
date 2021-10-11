module Components.Player

open Sutil
open Browser.Types


let Player (props: IStore<_>) (host: Node) = Html.article []

let register () =
    WebComponent.Register("sp-player", Player, ())
