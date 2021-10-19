module App

open Sutil
open Sutil.Attr
open Sutil.Styling

open Types
open Components
open Stores
open Browser.Types



let view () =
    let decorations =
        [ for _ in 0 .. Random.RNG.Next(5, 21) do
              Store.make (
                  { pos = Random.getPosition ()
                    kind = Random.getDecoration () }
              ) ]

    decorations
    |> List.iter
        (fun decoration ->
            Fable.Core.JS.setInterval
                (fun _ ->
                    Store.set
                        decoration
                        ({ pos = Random.getPosition ()
                           kind = Random.getDecoration () }))
                (Random.RNG.Next(4000, 5001))
            |> ignore)

    let onTouchStart (event: Event) =
        let event = (event :?> TouchEvent)
        Fable.Core.JS.console.log (event)

    let onTouchEnd (event: Event) =
        let event = (event :?> TouchEvent)
        Fable.Core.JS.console.log (event)

    let onTouchMove (event: Event) =
        let event = (event :?> TouchEvent)
        Fable.Core.JS.console.log (event)

    Html.app [
        for decoration in decorations do
            Bind.el (decoration, Store.make >> Decoration.element)
        Html.main [ Stage.element (StageStore) ]
        Html.footer []
        on "touchstart" onTouchStart []
        on "touchend" onTouchEnd []
        on "touchmove" onTouchMove []

        ]
