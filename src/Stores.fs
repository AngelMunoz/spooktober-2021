module Stores

open Sutil
open Types

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

        let allies = Random.getRandomNpcs (wave + 1 / 2) Ally

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
