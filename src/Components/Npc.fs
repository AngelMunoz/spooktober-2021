module Components.Npc

open Sutil
open Browser.Types


let Npc (props: IStore<_>) (host: Node) = Html.article []

let register () =
    WebComponent.Register("sp-npc", Npc, ())
