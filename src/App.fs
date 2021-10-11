module App

open Browser.Types
open Browser.Dom
open Sutil
open Sutil.DOM
open Sutil.Attr
open Sutil.Styling
open Sutil.Transition

open type Feliz.length

open Types
open Sutil
open Components

let private decorationTemplate (decoration: Decoration) =
    let kind = Store.make (decoration.kind)

    Html.custom (
        "sp-decoration",
        [ disposeOnUnmount [ kind ]
          Bind.attr ("kind", kind) ]
    )
    |> withStyle [
        rule
            "sp-decoration"
            [ Css.positionAbsolute
              Css.top decoration.pos.y
              Css.left decoration.pos.x ]
       ]

let maxY = window.innerHeight
let maxX = window.innerHeight

let random = System.Random()

let getRandomHeaderPos () =
    { x = random.Next(0, maxX - 32. |> int)
      y = random.Next(0, 100 - 32) }

let getRandomFooterPos () =
    { x = random.Next(0, maxX |> int)
      y = random.Next(maxY - 100. |> int, maxY - 32. |> int) }

let getRandomLeftPos () =
    { x = random.Next(0, 64 |> int)
      y = random.Next(0, maxY - 32. |> int) }

let getRandomRightPos () =
    { x = random.Next(maxX - 64. |> int, maxX |> int)
      y = random.Next(0, maxY - 32. |> int) }

let view () =
    let decorations =
        Store.make [
            for _ in 0 .. random.Next(0, 10) do
                { pos = getRandomHeaderPos ()
                  kind = Decoration.getRandomDecoration () }
            for _ in 0 .. random.Next(0, 10) do
                { pos = getRandomFooterPos ()
                  kind = Decoration.getRandomDecoration () }
            for _ in 0 .. random.Next(0, 10) do
                { pos = getRandomLeftPos ()
                  kind = Decoration.getRandomDecoration () }
            for _ in 0 .. random.Next(0, 10) do
                { pos = getRandomRightPos ()
                  kind = Decoration.getRandomDecoration () }
        ]

    Fable.Core.JS.setInterval
        (fun _ ->
            Store.set
                decorations
                [ for _ in 0 .. random.Next(0, 10) do
                    { pos = getRandomHeaderPos ()
                      kind = Decoration.getRandomDecoration () }
                  for _ in 0 .. random.Next(0, 10) do
                      { pos = getRandomFooterPos ()
                        kind = Decoration.getRandomDecoration () }
                  for _ in 0 .. random.Next(0, 10) do
                      { pos = getRandomLeftPos ()
                        kind = Decoration.getRandomDecoration () }
                  for _ in 0 .. random.Next(0, 10) do
                      { pos = getRandomRightPos ()
                        kind = Decoration.getRandomDecoration () } ])
        5000
    |> ignore

    Html.app [
        Html.header []
        Html.main [
            Html.div [
                Bind.each (decorations, decorationTemplate, [ fade |> withProps [ Duration 2500. ] |> InOut ])
            ]
        ]
        Html.footer []
    ]
