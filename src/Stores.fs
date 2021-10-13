module Stores

open Sutil
open Types

let StageStore = Store.make ()
let PlayerMovement: IStore<Movement option> = Store.make None
