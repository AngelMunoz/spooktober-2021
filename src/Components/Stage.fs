module Components.Stage

open Fable.Core
open Browser.Types

open Sutil
open Sutil.DOM
open Sutil.Attr

open Components
open Stores
open Types

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
    Html.app [
        class' "pos-stage game-over"
        Html.text "Game Over"
    ]

let gameTemplate player =


    Html.app [
        on "keydown" tryMovePlayer [ PreventDefault ]
        on "keyup" tryTrackAction [ PreventDefault ]
        Player.element player
        Bind.each (Enemies, Store.make >> Npc.element)
        Bind.each (Allies, Store.make >> Npc.element)
    ]


let element (props: IStore<Stage>) =

    let stageState = props .> (fun s -> s.state)

    let player =
        Store.make
            { life = 1000
              action = None
              pos = { x = 0; y = 0 } }

    let lifeSub =
        player.Subscribe(fun player -> if player.life <= 0 then Game.GameOver())

    let actionMonitor =
        (player .> (fun p -> p.action, p.pos))
        |> Observable.subscribe
            (fun (action, pos) ->
                let playerPos = (pos.x, pos.y)

                match action with
                | Some Attack ->
                    Enemies
                    <~= (fun enemies ->
                        enemies
                        |> List.map
                            (fun npc ->
                                let npcPos = (npc.pos.x, npc.pos.y)

                                if Position.isWithinDistance playerPos 10 npcPos then
                                    { npc with
                                          life = npc.life |> Option.map (fun life -> life - 100) }
                                else
                                    npc)
                        |> List.filter (fun npc -> (npc.life |> Option.defaultValue 0) > 0))
                | _ -> ())

    let damagePlayer (e: Event) =
        let e = (e :?> CustomEvent<{| position: Pos |}>)

        player
        <~= (fun player ->
            match e.detail with
            | Some npcPos ->
                let playerPos = (player.pos.x, player.pos.y)
                let npcPos = (npcPos.position.x, npcPos.position.y)

                if Position.isWithinDistance playerPos 10 npcPos then
                    let damage =
                        match player.action with
                        | Some Defend -> 100 - 50
                        | Some Slide -> 100 - 10
                        | _ -> 100

                    { player with
                          life = player.life - damage }
                else
                    player
            | None -> player)

    let healPlayer (e: Event) =
        let e = (e :?> CustomEvent<{| position: Pos |}>)

        player
        <~= (fun player ->
            match e.detail with
            | Some npcPos ->
                let playerPos = (player.pos.x, player.pos.y)
                let npcPos = (npcPos.position.x, npcPos.position.y)

                if Position.isWithinDistance playerPos 15 npcPos then
                    let heal =
                        match player.action with
                        | Some Defend -> 100 + 25
                        | Some Slide -> 100 - 25
                        | _ -> 100

                    { player with
                          life = player.life + heal }
                else
                    player
            | None -> player)

    Html.article [
        disposeOnUnmount [
            player
            lifeSub
            actionMonitor
        ]
        Attr.tabIndex 0
        class' "pos-stage"
        on "on-npc-attack" damagePlayer []
        on "on-npc-heal" healPlayer []
        Bind.el (
            stageState,
            fun state ->
                match state with
                | Idle -> idleTemplate props
                | GameOver -> gameOverTemplate props
                | Wave _ -> gameTemplate player
        )
    ]
