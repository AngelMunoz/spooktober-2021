module Components.Stage

open Sutil
open Sutil.Attr
open Browser.Types
open Components
open Stores
open Types

let element (props: IStore<_>) =

    let tryMovePlayer (event: KeyboardEvent) =
        Fable.Core.JS.console.log (event.key)

        match event.key with
        | "ArrowUp"
        | "w" -> PlayerMovement <~ Some Up
        | "ArrowDown"
        | "s" -> PlayerMovement <~ Some Down
        | "ArrowLeft"
        | "a" -> PlayerMovement <~ Some Left
        | "ArrowRight"
        | "d" -> PlayerMovement <~ Some Right
        | _ -> PlayerMovement <~ None

    Html.article [
        Attr.tabIndex 0
        class' "pos-stage"
        onKeyDown tryMovePlayer []
        on "keyup" (fun _ -> None |> Store.set PlayerMovement) []
        Player.element ()
    ]
