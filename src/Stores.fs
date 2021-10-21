module Stores

open Sutil
open Types

let StageStore =
    Store.make ({ state = Idle; points = 0 })

let Enemies = Store.make []

let Allies = Store.make []

let PlayerMovement: IStore<Movement option> = Store.make None
let PlayerActions: IStore<PlayerAction option> = Store.make None

module Game =
    let StartWave () =
        let state = StageStore |-> (fun s -> s.state)

        match state with
        | Wave number ->
            let enemies =
                Random.getRandomNpcs (number + 1 * 5) Enemy

            let allies =
                Random.getRandomNpcs (number + 1 / 2) Ally

            Enemies <~ enemies
            Allies <~ allies

            StageStore
            <~= (fun store -> { store with state = Wave(number + 1) })
        | Idle
        | GameOver ->
            let enemies = Random.getRandomNpcs (1 * 5) Enemy

            let allies = Random.getRandomNpcs (1 / 2) Ally
            Enemies <~ enemies
            Allies <~ allies

            StageStore
            <~= (fun store ->
                { store with
                      state = Wave 1
                      points = 0 })

    let private cleanNpcs () =
        Enemies <~ []
        Allies <~ []

    let GameOver () =
        StageStore
        <~= (fun store -> { store with state = GameOver })

    let EndGame () =
        cleanNpcs ()

        StageStore
        <~= (fun store -> { store with state = Idle; points = 0 })
