module App

open Sutil
open Sutil.Styling

open Types
open Components
open Stores



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

    Html.app [
        for decoration in decorations do
            Bind.el (decoration, Decoration.element)
        Html.main [ Stage.element (StageStore) ]
        Html.footer []
    ]
