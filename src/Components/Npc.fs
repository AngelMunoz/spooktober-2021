module Components.Npc

open System
open Sutil
open Sutil.DOM
open Browser.Types
open Types
open Sutil.Attr

let private randomMovement (npc: IStore<Npc>) =
    let randomPosNegNumber () =
        (Random.RNG.Next(10)
         * if Random.RNG.Next(0, 2) = 0 then
               1
           else
               -1)

    let updatePosition (npc: Npc) =
        let x = randomPosNegNumber ()
        let y = randomPosNegNumber ()

        { npc with
              pos = { x = npc.pos.x + x; y = npc.pos.y + y } }

    npc <~= updatePosition

let private setAttackInterval (el: HTMLElement) (store: IStore<Npc>) =
    fun () ->
        randomMovement store

        CustomDispatch.toCustomEvent [
            Bubbles true
            Composed true
            Detail(
                {| position = store |-> (fun p -> p.pos) |}
                |> Some
            )
        ]
        |> dispatchCustom el "on-npc-attack"

        el.classList.add ("slime--attack")

        Fable.Core.JS.setTimeout (fun _ -> el.classList.remove ("slime--attack")) 200
        |> ignore

let private setHealInterval (el: HTMLElement) (store: IStore<Npc>) =
    fun () ->
        CustomDispatch.toCustomEvent [
            Bubbles true
            Composed true
            Detail(
                {| position = store |-> (fun p -> p.pos) |}
                |> Some
            )
        ]
        |> dispatchCustom el "on-npc-heal"

        el.classList.add ("slime--heal")

        Fable.Core.JS.setTimeout (fun _ -> el.classList.remove ("slime--heal")) 1000
        |> ignore

let element (props: IStore<Npc>) =
    let position = props .> (fun p -> p.pos)
    let kind = props .> (fun p -> p.kind)
    let disposables = ResizeArray<IDisposable>()
    let fnInterval = Random.RNG.Next(500, 2001)

    let mounted (e: Event) =
        let target = e.target :?> HTMLElement

        let interval = Timing.interval fnInterval

        match props |-> (fun p -> p.kind) with
        | Enemy -> disposables.Add(setAttackInterval target props |> interval)
        | Ally -> disposables.Add(setHealInterval target props |> interval)
        | Neutral -> ()


    Html.article [
        onMount mounted []
        disposeOnUnmount [
            { new IDisposable with
                override _.Dispose() : unit =
                    disposables |> Seq.iter (fun s -> s.Dispose()) }
            props
        ]
        Bind.el (
            kind,
            fun kind ->
                match kind with
                | Enemy -> "slime__orange"
                | Ally -> "slime__green"
                | Neutral -> "slime__white"
                |> sprintf "npc slime %s slime--down"
                |> class'
        )
        Bind.el (
            position,
            fun position ->
                $"top: {position.y}px; left: {position.x}px"
                |> Attr.style
        )

        ]
