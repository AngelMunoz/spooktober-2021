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
