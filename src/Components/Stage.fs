module Components.Stage

open Sutil
open Sutil.Attr
open Browser.Types


let element (props: IStore<_>) = Html.article [ class' "pos-stage" ]
