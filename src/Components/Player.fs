module Components.Player

open Sutil
open Sutil.Attr
open Browser.Types

open Types
open Stores
open Sutil.DOM

let element maxLife (player: Player) =
    let player = Store.make player

    let position =
        player
        .> (fun player -> $"left: {player.pos.x}px; top: {player.pos.y}px;")

    let life = player .> (fun player -> player.life)

    let status =
        PlayerMovement
        .> (fun movement ->
            match movement with
            | None -> Idle
            | Some movement -> Moving movement)

    let classes =
        (PlayerActions, status) ||> Observable.zip
        .> (fun (action, status) ->
            let status =
                match status with
                | Idle -> "slime--down"
                | Moving Down -> "slime--down slime--moving"
                | Moving Up -> "slime--up slime--moving"
                | Moving Left -> "slime--left slime--moving"
                | Moving Right -> "slime--right slime--moving"

            let actions =
                match action with
                | Some Slide -> "slime--slide"
                | Some Attack -> "slime--attack"
                | Some Defend -> "slime--defend"
                | None -> ""

            $"player slime {status} {actions}")

    Html.div [
        disposeOnUnmount [ player ]
        Bind.attr ("class", classes)
        Bind.attr ("style", position)
        Healthbar.element maxLife life
    ]
