module Styles

open Sutil
open Sutil.Styling
open Types

let private bgDecorationPos (kind: DecorationKind) =
    let spriteWidth = 32

    match kind with
    | Flask0 -> 0
    | Flask1 -> spriteWidth
    | Flask2 -> spriteWidth * 2
    | Flask3 -> spriteWidth * 3
    | Flask4 -> spriteWidth * 4
    | Flask5 -> spriteWidth * 5
    | Pumpkin0Back -> spriteWidth * 6
    | Pumpkin1Back -> spriteWidth * 7
    | Pumpkin2Back -> spriteWidth * 8
    | Pumpkin0Front -> spriteWidth * 9
    | Pumpkin1Front -> spriteWidth * 10
    | Pumpkin2Front -> spriteWidth * 11
    | Lollipop -> spriteWidth * 12
    | Candy0 -> spriteWidth * 13
    | Candy1 -> spriteWidth * 14
    | CandyBag -> spriteWidth * 15
    |> (fun spritePos -> spritePos * -1)

let decoration (kind: DecorationKind) =
    [ rule
          "div"
          [ Css.backgroundImageUrl "/assets/32x32_Halloween_Free.png"
            Css.custom ("background-position", $"%i{bgDecorationPos kind}px 0px")
            Css.backgroundRepeatNoRepeat
            Css.height 32
            Css.width 32
            Css.transformScale 1.5 ] ]
