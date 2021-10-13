module Components.Slime


open Browser.Types

open Sutil
open Sutil.Styling
open Sutil.Attr

open Types


let element () = Html.div [ class' "slime slime--down" ]
