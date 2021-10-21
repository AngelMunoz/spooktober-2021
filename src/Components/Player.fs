module Components.Player

open Sutil
open Sutil.Attr
open Browser.Types

open Types
open Stores
open Sutil.DOM

type PlayerStatus =
    | Idle
    | Moving of Movement

let element (player: IStore<Player>) =

    let status =
        PlayerMovement
        .> (fun movement ->
            match movement with
            | None -> Idle
            | Some movement -> Moving movement)

    let playerPos =
        (PlayerActions, status) ||> Observable.zip
        .> (fun (action, status) ->
            let maxXY = Position.getMaxXY ()
            let x = player |-> (fun player -> player.pos.x)
            let y = player |-> (fun player -> player.pos.y)

            let dx, dy =
                match status with
                | Idle -> x, y
                | Moving Down ->
                    match action with
                    | Some Slide -> x, y + 30
                    | _ -> x, y + 10
                | Moving Up ->
                    match action with
                    | Some Slide -> x, y - 30
                    | _ -> x, y - 10
                | Moving Left ->
                    match action with
                    | Some Slide -> x - 30, y
                    | _ -> x - 10, y
                | Moving Right ->
                    match action with
                    | Some Slide -> x + 30, y
                    | _ -> x + 10, y

            let (dx, dy) = Position.getClampPos (dx, dy) maxXY

            player
            <~= (fun player -> { player with pos = { x = dx; y = dy } })

            $"left: {dx}px; top: {dy}px;")

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

    let statusSub =
        PlayerActions.Subscribe
            (fun action ->
                player
                <~= (fun player -> { player with action = action }))

    Html.div [
        disposeOnUnmount [ statusSub ]
        Bind.attr ("class", classes)
        Bind.attr ("style", playerPos)
    ]
