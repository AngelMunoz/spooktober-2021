module Components.Stage

open Sutil
open Sutil.Attr
open Browser.Types
open Components
open Stores
open Types

let element (props: IStore<_>) =

    let tryMovePlayer (event: Event) =
        let event = event :?> KeyboardEvent
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
        | key ->
            PlayerMovement <~ None
            match key with
            | "Meta"
            | "j" -> PlayerActions <~ Some Attack
            | "Control"
            | "l" -> PlayerActions <~ Some Defend
            | "Alt"
            | "k" -> PlayerActions <~ Some Slide
            | _ -> PlayerActions <~ None

    let tryTrackAction (event: Event) =
        let event = event :?> KeyboardEvent
        Fable.Core.JS.console.log (event.key)

        match event.key with
        | "Meta"
        | "j" 
        | "Control"
        | "l"
        | "Alt"
        | "k" -> PlayerActions <~ None
        | "ArrowUp"
        | "w"
        | "ArrowDown"
        | "s"
        | "ArrowLeft"
        | "a"
        | "ArrowRight"
        | "d" -> PlayerMovement <~ None
        | _ -> ()

    Html.article [
        Attr.tabIndex 0
        class' "pos-stage"
        on "keydown" tryMovePlayer [ PreventDefault ]
        on "keyup" tryTrackAction [ PreventDefault ]
        Player.element ()
    ]
