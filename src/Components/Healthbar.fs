module Components.Healthbar

open System
open Sutil
open Sutil.DOM
open Browser.Types
open Types
open Sutil.Attr


let element (max: int) (health: IObservable<int>) =

    Html.meter [
        class' "healthbar"
        Attr.min 0
        Attr.max max
        Attr.low (((max |> float) * 0.20))
        Attr.high (((max |> float) * 0.75))
        Bind.attr ("value", health)
    ]
