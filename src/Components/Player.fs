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

let element () =
    let mutable x = 0
    let mutable y = 0

    let status =
        PlayerMovement
        .> (fun movement ->
            match movement with
            | None -> Idle
            | Some movement -> Moving movement)

    let playerPos =
        (PlayerActions, status) ||> Observable.zip
        .> (fun (action, status) ->
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

            x <- dx
            y <- dy
            $"top: {y}px; left: {x}px;")

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
        Bind.attr ("class", classes)
        Bind.attr ("style", playerPos)
    ]
