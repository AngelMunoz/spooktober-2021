[<AutoOpen>]
module Extensions

open System

[<RequireQualifiedAccess>]
module Timing =
    let interval time cb =
        let id = Fable.Core.JS.setInterval cb time

        { new IDisposable with
            override _.Dispose() : unit = Fable.Core.JS.clearInterval id }

    let timeout time cb =
        let id = Fable.Core.JS.setTimeout cb time

        { new IDisposable with
            override _.Dispose() : unit = Fable.Core.JS.clearTimeout id }


[<RequireQualifiedAccess>]
module Position =

    let distanceBetween (x1: int, y1: int) (x2: int, y2: int) =
        let d1 = Math.Pow((x2 - x1) |> float, 2.)
        let d2 = Math.Pow((y2 - y1) |> float, 2.)
        Math.Sqrt(d1 + d2) |> int


    let isWithinDistance origin maxDistance position =
        (distanceBetween origin position) <= maxDistance

    let getMaxXY () =
        let stage =
            Browser.Dom.document.querySelector (".pos-stage")

        ((stage.clientWidth |> int) - 22, (stage.clientHeight |> int) - 26)

    let getClampPos (x: int, y: int) (maxX: int, maxY: int) =
        let x =
            if x <= 0 then 0
            elif x >= maxX then maxX
            else x

        let y =
            if y <= 0 then 0
            elif y >= maxY then maxY
            else y

        (x, y)
