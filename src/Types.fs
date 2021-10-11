module Types

open Fable.Core

type Pos = { x: int; y: int }

type NpcKind =
    | Enemy
    | Ally
    | Neutral

type Npc =
    { life: int option
      pos: Pos
      kind: NpcKind }

type Player = { life: int; mana: int; pos: Pos }


[<StringEnum>]
type DecorationKind =
    | Flask0
    | Flask1
    | Flask2
    | Flask3
    | Flask4
    | Flask5
    | Pumpkin0Back
    | Pumpkin1Back
    | Pumpkin2Back
    | Pumpkin0Front
    | Pumpkin1Front
    | Pumpkin2Front
    | Lollipop
    | Candy0
    | Candy1
    | CandyBag


type Decoration = { pos: Pos; kind: DecorationKind }
