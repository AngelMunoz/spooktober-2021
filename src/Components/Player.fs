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
        status
        .> (fun status ->
            let dx, dy =
                match status with
                | Idle -> x, y
                | Moving Down -> x, y + 10
                | Moving Up -> x, y - 10
                | Moving Left -> x - 10, y
                | Moving Right -> x + 10, y

            x <- dx
            y <- dy
            $"top: {y}px; left: {x}px;")

    let classes =
        status
        .> (fun status ->
            let status =
                match status with
                | Idle -> "slime--down"
                | Moving Down -> "slime--down slime--moving"
                | Moving Up -> "slime--up slime--moving"
                | Moving Left -> "slime--left slime--moving"
                | Moving Right -> "slime--right slime--moving"

            $"player slime {status}")


    Html.div [
        Bind.attr ("class", classes)
        Bind.attr ("style", playerPos)
    ]
