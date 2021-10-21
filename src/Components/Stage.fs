module Components.Stage

open Sutil
open Sutil.Attr
open Browser.Types
open Components
open Stores
open Types
open Fable.Core

let private tryMovePlayer (event: Event) =
    let event = event :?> KeyboardEvent

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
        | "Shift"
        | "j" -> PlayerActions <~ Some Attack
        | "Control"
        | "l" -> PlayerActions <~ Some Defend
        | "Alt"
        | "k" -> PlayerActions <~ Some Slide
        | _ -> PlayerActions <~ None

let private tryTrackAction (event: Event) =
    let event = event :?> KeyboardEvent
    Fable.Core.JS.console.log (event.key)

    match event.key with
    | "Shift"
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

[<Emit("document.querySelector('.pos-stage')?.focus()")>]
let focusStage () : unit = jsNative

let idleTemplate props =
    let onStart (e: Event) =
        Game.StartWave()
        focusStage ()

    Html.app [
        Html.article [
            class' "idle"
            Html.button [
                type' "button"
                class' "nes-btn"
                Html.text "Start"
                onClick onStart []
            ]
        ]
    ]

let gameOverTemplate props =
    Html.parent "article" [ class' "pos-stage game-over" ]

let gameTemplate props =

    Html.app [
        on "keydown" tryMovePlayer [ PreventDefault ]
        on "keyup" tryTrackAction [ PreventDefault ]
        Player.element ()
        Bind.each (Enemies, Store.make >> Npc.element)
        Bind.each (Allies, Store.make >> Npc.element)
    ]


let element (props: IStore<Stage>) =

    let stageState = props .> (fun s -> s.state)

    Html.article [
        Attr.tabIndex 0
        class' "pos-stage"
        Bind.el (
            stageState,
            fun state ->
                match state with
                | Idle -> idleTemplate props
                | GameOver -> gameOverTemplate props
                | Wave _ -> gameTemplate props
        )
    ]
