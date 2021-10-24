module Stores

open System
open Browser.Types
open Fable.Core
open Sutil
open Sutil
open Sutil.DOM
open Types


[<Emit("new EventTarget()")>]
let makeEventTarget () : EventTarget = jsNative

let StageStore =
    Store.make (
        { state = Start
          points = 0
          maxLife = 1000 }
    )

let Enemies = Store.make []

let Allies = Store.make []

let PlayerMovement: IStore<Movement option> = Store.make None
let PlayerActions: IStore<PlayerAction option> = Store.make None

let private playerBus = makeEventTarget ()

type PlayerEventBus() =

    static member OnMove(onMove: CustomEvent<int * int> -> unit) =
        let cb (event: Event) =
            onMove (event :?> CustomEvent<int * int>)

        playerBus.addEventListener ("on-player-move", cb)

        { new IDisposable with
            member _.Dispose() =
                playerBus.removeEventListener ("on-player-move", cb) }

    static member Move(x: int, y: int) =
        CustomDispatch.toCustomEvent [
            Bubbles true
            Composed true
            (x, y) |> Some |> Detail
        ]
        |> dispatchCustom playerBus "on-player-move"

module Game =
    let private cleanNpcs () =
        Enemies <~ []
        Allies <~ []

    let StartWave (wave: int) =
        let state = StageStore |-> (fun s -> s.state)

        match state with
        | InterWave _ -> cleanNpcs ()
        | Start
        | Wave _ ->
            let enemies =
                Random.getRandomNpcs (wave + 1 * 5) Enemy

            let allies = Random.getRandomNpcs (wave + 1 / 2) Ally

            Enemies <~ enemies
            Allies <~ allies

            StageStore
            <~= (fun store -> { store with state = Wave(wave + 1) })
        | GameOver -> cleanNpcs ()

    let NextWave wave =
        let enemies =
            Random.getRandomNpcs (wave + 1 * 5) Enemy

        let allies =
                if wave >= 8 then
                    Random.getRandomNpcs (wave + 1 / 2) Ally
                else []

        Enemies <~ enemies
        Allies <~ allies

        StageStore
        <~= (fun store -> { store with state = Wave(wave + 1) })

    let SetStart () =
        StageStore
        <~= (fun store -> { store with state = Start })

    let SetInterwave wave =
        StageStore
        <~= (fun store -> { store with state = InterWave wave })


    let GameOver () =
        StageStore
        <~= (fun store -> { store with state = GameOver })

    let EndGame () =
        cleanNpcs ()

        StageStore
        <~= (fun store -> { store with state = Start; points = 0 })
