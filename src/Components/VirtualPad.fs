module Components.VirtualPad

open Sutil
open Sutil.Attr
open Browser.Types

open Types
open Stores
open Sutil.DOM
open Sutil.Styling

type PadKind =
    | Movement
    | Action

let private getMovementCls (item: Movement option) =
    match item with
    | Some Up -> "pad--up"
    | Some Down -> "pad--down"
    | Some Left -> "pad--left"
    | Some Right -> "pad--right"
    | None -> ""
    |> sprintf "direction-pad %s"

let private getActionCls (item: PlayerAction option) =
    match item with
    | Some Attack -> "action--attack"
    | Some Defend -> "action--defend"
    | Some Slide -> "action--slide"
    | None -> ""
    |> sprintf "action-pad %s"

let element () =
    let action = Store.make None
    let movement = Store.make None

    let actionCls = action .> getActionCls
    let movementCls = movement .> getMovementCls

    let updateActions =
        PlayerActions.Subscribe(fun plAction -> action <~ plAction)

    let updateMovement =
        PlayerMovement.Subscribe(fun plMovement -> movement <~ plMovement)

    let actionClick (action: PlayerAction) _ = PlayerActions <~ Some action

    let movementClick (movement: Movement) _ = PlayerMovement <~ Some movement

    let leaveButton (kind: PadKind) _ =
        match kind with
        | Movement -> PlayerMovement <~ None
        | Action -> PlayerActions <~ None

    Html.article [
        disposeOnUnmount [
            action
            movement
            updateActions
            updateMovement
        ]
        class' "virtual-pad"
        Html.section [
            Bind.className movementCls
            Html.button [
                type' "button"
                class' "nes-btn"
                Html.text "w"
                on "touchstart" (movementClick Up) []
                on "pointerdown" (movementClick Up) []
                on "pointerup" (leaveButton Movement) []
                on "touchend" (leaveButton Movement) []
            ]
            Html.button [
                type' "button"
                class' "nes-btn"
                Html.text "a"
                on "touchstart" (movementClick Left) []
                on "pointerdown" (movementClick Left) []
                on "pointerup" (leaveButton Movement) []
                on "touchend" (leaveButton Movement) []
            ]
            Html.button [
                type' "button"
                class' "nes-btn"
                Html.text "d"
                on "touchstart" (movementClick Right) []
                on "pointerdown" (movementClick Right) []
                on "pointerup" (leaveButton Movement) []
                on "touchend" (leaveButton Movement) []
            ]
            Html.button [
                type' "button"
                class' "nes-btn"
                Html.text "s"
                on "touchstart" (movementClick Down) []
                on "pointerdown" (movementClick Down) []
                on "pointerup" (leaveButton Movement) []
                on "touchend" (leaveButton Movement) []
            ]
        ]
        Html.section [
            Bind.className actionCls

            Html.button [
                type' "button"
                class' "nes-btn is-error"
                on "touchstart" (actionClick Attack) []
                on "pointerdown" (actionClick Attack) []
                on "pointerup" (leaveButton Action) []
                on "touchend" (leaveButton Action) []
            ]
            Html.button [
                type' "button"
                class' "nes-btn"
                on "touchstart" (actionClick Defend) []
                on "pointerdown" (actionClick Defend) []
                on "pointerup" (leaveButton Action) []
                on "touchend" (leaveButton Action) []
            ]
            Html.button [
                type' "button"
                class' "nes-btn is-success"
                on "touchstart" (actionClick Slide) []
                on "pointerdown" (actionClick Slide) []
                on "pointerup" (leaveButton Action) []
                on "touchend" (leaveButton Action) []
            ]
        ]
    ]
