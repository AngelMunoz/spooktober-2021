module Stores

open Sutil
open Types

let StageStore = Store.make ()

let Enemies =
    Store.make [
        { pos = { x = 100; y = 100 }
          kind = Enemy
          life = Some 100 }
        { pos = { x = 150; y = 150 }
          kind = Enemy
          life = Some 100 }
        { pos = { x = 50; y = 50 }
          kind = Enemy
          life = Some 100 }
        { pos = { x = 250; y = 250 }
          kind = Enemy
          life = Some 100 }
    ]

let Allies =
    Store.make [
        { pos = { x = 200; y = 75 }
          kind = Ally
          life = Some 100 }
        { pos = { x = 150; y = 300 }
          kind = Ally
          life = Some 100 }
    ]

let PlayerMovement: IStore<Movement option> = Store.make None
let PlayerActions: IStore<PlayerAction option> = Store.make None
