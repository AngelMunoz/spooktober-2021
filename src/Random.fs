[<RequireQualifiedAccess>]
module Random

open System
open Types
open Browser.Dom

let private maxY = window.innerHeight
let private maxX = window.innerWidth

let RNG = Random()

let getHeaderPosition () =
    { x = RNG.Next(0, maxX - 32. |> int)
      y = RNG.Next(0, 100 - 32) }

let getFooterPosition () =
    { x = RNG.Next(0, maxX |> int)
      y = RNG.Next(maxY - 100. |> int, maxY - 32. |> int) }

let getLeftPosition () =
    { x = RNG.Next(0, 64 |> int)
      y = RNG.Next(0, maxY - 32. |> int) }

let getRightPosition () =
    { x = RNG.Next(maxX - 64. |> int, maxX |> int)
      y = RNG.Next(0, maxY - 32. |> int) }

let getPosition () =
    match RNG.Next(0, 5) with
    | 0 -> getHeaderPosition ()
    | 1 -> getFooterPosition ()
    | 2 -> getLeftPosition ()
    | 3 -> getRightPosition ()
    | _ -> getHeaderPosition ()

let getDecoration () =
    match RNG.Next(16) with
    | 0 -> Flask0
    | 1 -> Flask1
    | 2 -> Flask2
    | 3 -> Flask3
    | 4 -> Flask4
    | 5 -> Flask5
    | 6 -> Pumpkin0Back
    | 7 -> Pumpkin1Back
    | 8 -> Pumpkin2Back
    | 9 -> Pumpkin0Front
    | 10 -> Pumpkin1Front
    | 11 -> Pumpkin2Front
    | 12 -> Lollipop
    | 13 -> Candy0
    | 14 -> Candy1
    | 15 -> CandyBag
    | _ -> Flask0
