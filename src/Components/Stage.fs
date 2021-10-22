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

let startGameTemplate props =
    let onStart (e: Event) =
        Game.StartWave 0
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

let interWaveTemplate (props: IStore<Stage>, wave: int, player: IStore<Player>) =
    let onStart (e: Event) =
        Game.NextWave wave

        player
        <~= (fun player ->
            { player with
                  pos = { x = 0; y = 0 }
                  action = None })

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

let gameOverTemplate props =
    Html.app [
        class' "pos-stage game-over"
        Html.text "Game Over"
    ]



let element (props: IStore<Stage>) =

    let stageState = props .> (fun s -> s.state)

    let gameTemplate (player: IStore<Player>) =
        Html.app [
            Bind.el (player, Player.element props.Value.maxLife)
            Bind.each (Enemies, Store.make >> (Npc.element props.Value.maxLife))
            Bind.each (Allies, Store.make >> (Npc.element props.Value.maxLife))

            ]

    let player =
        Store.make
            { life = props.Value.maxLife
              action = None
              pos = { x = 0; y = 0 } }

    let lifeSub =
        player.Subscribe(fun player -> if player.life <= 0 then Game.GameOver())

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
                            { player with
                                  life = props.Value.maxLife })
                | _ -> ())

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

                                if Position.isWithinDistance playerPos 25 npcPos then
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

                if Position.isWithinDistance playerPos 25 npcPos then
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

                if Position.isWithinDistance playerPos 20 npcPos then
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
            | None -> player)

    let trackPlayer =
        PlayerEventBus.OnMove
            (fun event ->
                match event.detail with
                | Some (x, y) ->
                    Fable.Core.JS.console.log (x, y)

                    player
                    <~= (fun player -> { player with pos = { x = x; y = y } })
                | None -> ())

    let statusSub =
        PlayerActions.Subscribe
            (fun action ->
                player
                <~= (fun player -> { player with action = action }))

    let playerPos =
        (props, ((PlayerActions, PlayerMovement) ||> Observable.zip))
        ||> Observable.zip
        |> Observable.subscribe
            (fun (stage, (action, status)) ->
                match stage.state with
                | Wave _ ->
                    let maxXY = Position.getMaxXY ()
                    let x = player |-> (fun player -> player.pos.x)
                    let y = player |-> (fun player -> player.pos.y)

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
        Bind.el (
            stageState,
            fun state ->
                match state with
                | Start -> startGameTemplate props
                | GameOver -> gameOverTemplate props
                | InterWave wave -> interWaveTemplate (props, wave, player)
                | Wave _ -> gameTemplate player
        )
    ]
