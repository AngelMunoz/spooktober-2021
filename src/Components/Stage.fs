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
let private focusStage () : unit = jsNative

let private startGameTemplate (props: IStore<Stage>) (player: IStore<Player option>) =
    let onStart (e: Event) =
        Game.StartWave 0
        focusStage ()
    player
    <~ ({ life = props.Value.maxLife
          action = None
          pos = { x = 0; y = 0 } }
        |> Some)

    Html.app [
        Html.article [
            class' "idle"
            Html.p [
                Html.text "Hello! this is a simple'ish game with"
                Html.a [
                    Attr.href "https://fsharp.org/"
                    Html.text " F#"
                ]
                Html.text " and "
                Html.a [
                    Attr.href "https://sutil.dev"
                    Html.text " Sutil"
                ]
            ]
            Html.section [
                Html.p
                    """
                    Try to remove all of the enemy slimes which are yellow colour,
                    On a Pc use the keyboard arrows to move, on mobile use the pad below
                    """
                Html.ul [
                    Html.li "Shift - Red - Attack"
                    Html.li "Ctrl - White - Defend"
                    Html.li "Alt - Green - Run"
                ]
            ]
            Html.button [
                type' "button"
                class' "nes-btn"
                Html.text "Start"
                onClick onStart []
            ]
        ]
    ]

let private interWaveTemplate (props: IStore<Stage>, wave: int, player: IStore<Player option>) =
    let onStart (e: Event) =
        Game.NextWave wave

        player
        <~= (fun player ->
            match player with
            | None ->
                { life = props.Value.maxLife
                  action = None
                  pos = { x = 0; y = 0 } }
                |> Some
            | Some player ->
                { player with
                      action = None
                      pos = { x = 0; y = 0 } }
                |> Some)

        focusStage ()

    Html.article [
        class' "idle"
        Html.button [
            type' "button"
            class' "nes-btn"
            Html.text "Next Wave"
            onClick onStart []
        ]
    ]

let private gameOverTemplate props lastWave =
    let onStartOver (e: Event) =
        Game.SetStart()
        Game.NextWave 0
        
    Html.article [
        class' "game-over"
        Html.h1 "Game Over!"
        Html.p $"You made it to wave: %i{lastWave}"
        Html.button [
            type' "button"
            class' "nes-btn"
            Html.text "Start Over"
            onClick onStartOver []
        ]
    ]
    

let element (props: IStore<Stage>) =
    let mutable lastWave = 1
    let stageState = props .> (fun s -> s.state)

    let gameTemplate (player: IStore<Player option>) =
        Html.app [
            Bind.el (
                player,
                fun player ->
                    match player with
                    | Some player -> Player.element props.Value.maxLife player
                    | None -> Html.none
            )
            Bind.each (Enemies, Store.make >> (Npc.element props.Value.maxLife))
            Bind.each (Allies, Store.make >> (Npc.element props.Value.maxLife))

            ]
    
    let waveTpl (wave: int option) =
        Html.section [
            class' "wave-indicator"
            match wave with
            | Some wave ->
                Html.p $"Current Wave: %i{wave}"
            | None -> Html.none
        ]
    
    let player = Store.make None

    let lifeSub =
        player
        |> Observable.filter Option.isSome
        |> Observable.subscribe
            (fun currentPlayer ->
                match currentPlayer with
                | Some pl ->
                    if pl.life <= 0 then
                        player <~ None
                        let state = props |-> (fun p -> p.state)
                        match state with
                        | Wave number -> lastWave <- number
                        | _ -> ()
                        Game.GameOver()
                | None -> ())

    let enemyMonitor =
        (Enemies, StageStore)
        ||> Observable.zip
        |> Observable.distinctUntilChanged
        |> Observable.subscribe
            (fun (enemies, store) ->

                Fable.Core.JS.console.log (enemies |> List.toArray, store)

                match store.state with
                | Wave wave ->
                    if enemies.Length = 0 then
                        Game.SetInterwave wave
                | _ -> ())

    let waveMonitor =
        StageStore.Subscribe
            (fun store ->
                match store.state with
                | Wave number ->
                    if (number / 5) % 2 = 0 then
                        player
                        <~= (fun player ->
                            match player with
                            | Some player ->
                                { player with
                                      life = props.Value.maxLife }
                                |> Some
                            | None -> None)
                | _ -> ())

    let actionMonitor =
        (player
         .> (fun p -> p |> Option.map (fun p -> p.action, p.pos)))
        |> Observable.subscribe
            (fun values ->
                match values with
                | Some (action, pos) ->
                    let playerPos = (pos.x, pos.y)

                    let attackNpc (npc: Npc) =
                        let npcPos = (npc.pos.x, npc.pos.y)

                        if Position.isWithinDistance playerPos 50 npcPos then
                            { npc with
                                  life = npc.life |> Option.map (fun life -> life - 100) }
                        else
                            npc

                    let filterAlive (npc: Npc) =
                        let npcLife = npc.life |> Option.defaultValue 0
                        npcLife > 0

                    match action with
                    | Some Attack ->
                        Enemies
                        <~= (fun enemies ->
                            enemies
                            |> List.map attackNpc
                            |> List.filter filterAlive)
                    | _ -> ()
                | None -> ())

    let damagePlayer (e: Event) =
        let e = (e :?> CustomEvent<{| position: Pos |}>)

        player
        <~= (fun player ->
            match e.detail with
            | Some npcPos ->
                match player with
                | Some player ->
                    let playerPos = (player.pos.x, player.pos.y)
                    let npcPos = (npcPos.position.x, npcPos.position.y)

                    if Position.isWithinDistance playerPos 50 npcPos then
                        let damage =
                            match player.action with
                            | Some Defend -> 100 - 50
                            | Some Slide -> 100 - 10
                            | _ -> 100

                        { player with
                              life = player.life - damage }
                    else
                        player
                    |> Some
                | None -> None
            | None -> player)

    let healPlayer (e: Event) =
        let e = (e :?> CustomEvent<{| position: Pos |}>)

        player
        <~= (fun player ->
            match e.detail with
            | Some npcPos ->
                match player with
                | Some player ->
                    let playerPos = (player.pos.x, player.pos.y)
                    let npcPos = (npcPos.position.x, npcPos.position.y)

                    if Position.isWithinDistance playerPos 35 npcPos then
                        let heal =
                            match player.action with
                            | Some Defend -> 100 + 25
                            | Some Slide -> 100 - 25
                            | _ -> 100

                        let life =
                            if heal + player.life >= props.Value.maxLife then
                                props.Value.maxLife
                            else
                                heal + player.life

                        { player with life = life }
                    else
                        player
                    |> Some
                | None -> None
            | None -> player)

    let trackPlayer =
        PlayerEventBus.OnMove
            (fun event ->
                match event.detail with
                | Some (x, y) ->
                    player
                    <~= (fun player ->
                        player
                        |> Option.map (fun player -> { player with pos = { x = x; y = y } }))
                | None -> ())

    let statusSub =
        PlayerActions.Subscribe
            (fun action ->
                player
                <~= (fun player ->
                    player
                    |> Option.map (fun player -> { player with action = action })))

    let playerPos =
        (props, ((PlayerActions, PlayerMovement) ||> Observable.zip))
        ||> Observable.zip
        |> Observable.subscribe
            (fun (stage, (action, status)) ->
                match stage.state with
                | Wave _ ->
                    let maxXY = Position.getMaxXY ()

                    let x, y =
                        player
                        |-> (fun player ->
                            player
                            |> Option.map (fun p -> p.pos.x, p.pos.y)
                            |> Option.defaultValue (0, 0))

                    let dx, dy =
                        match status with
                        | None -> x, y
                        | Some Down ->
                            match action with
                            | Some Slide -> x, y + 30
                            | _ -> x, y + 10
                        | Some Up ->
                            match action with
                            | Some Slide -> x, y - 30
                            | _ -> x, y - 10
                        | Some Left ->
                            match action with
                            | Some Slide -> x - 30, y
                            | _ -> x - 10, y
                        | Some Right ->
                            match action with
                            | Some Slide -> x + 30, y
                            | _ -> x + 10, y

                    let (dx, dy) = Position.getClampPos (dx, dy) maxXY
                    PlayerEventBus.Move(dx, dy)
                | _ -> ())
    let wave = props .> (fun props -> match props.state with | Wave num -> Some num | _ -> None)
    Html.article [
        disposeOnUnmount [
            player
            lifeSub
            actionMonitor
            enemyMonitor
            waveMonitor
            trackPlayer
            statusSub
            playerPos
        ]
        Attr.tabIndex 0
        class' "pos-stage"
        on "on-npc-attack" damagePlayer []
        on "on-npc-heal" healPlayer []
        on "keydown" tryMovePlayer [ PreventDefault ]
        on "keyup" tryTrackAction [ PreventDefault ]
        
        Bind.el(wave, waveTpl)
        Bind.el (
            stageState,
            fun state ->
                match state with
                | Start -> startGameTemplate props player
                | GameOver -> gameOverTemplate props lastWave
                | InterWave wave -> interWaveTemplate (props, wave, player)
                | Wave _ -> gameTemplate player
        )
    ]
