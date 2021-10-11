module Components.Decoration

open Browser.Types

open Sutil
open Sutil.Styling
open Types

let private random = System.Random()

let private Decoration (props: IStore<Decoration>) (host: Node) =
    let kind = props .> (fun props -> props.kind)

    Html.div [
        Bind.el (kind, (fun kind -> adoptStyleSheet (Styles.decoration kind)))
    ]

let getRandomDecoration () =
    match random.Next(16) with
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


let register () =
    WebComponent.Register(
        "sp-decoration",
        Decoration,
        { pos = { x = 0; y = 0 }
          kind = Flask0 }
    )
