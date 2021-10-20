[<AutoOpen>]
module Extensions

open System

module Timing =
    let interval time cb =
        let id = Fable.Core.JS.setInterval cb time

        { new IDisposable with
            override _.Dispose() : unit = Fable.Core.JS.clearInterval id }

    let timeout time cb =
        let id = Fable.Core.JS.setTimeout cb time

        { new IDisposable with
            override _.Dispose() : unit = Fable.Core.JS.clearTimeout id }


module Position =
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
