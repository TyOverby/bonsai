open! Core_kernel
open! Bonsai_web
open Bonsai.Let_syntax

let component =
  let%sub counter_state = Bonsai.state [%here] (module Int) ~default_model:0 in
  return
  @@ let%map counter_value, set_counter = counter_state in
     Vdom.Node.div
       []
       [ Vdom.Node.textf "%d" counter_value
       ; Vdom.Node.button
           [ Vdom.Attr.on_click (fun _ -> set_counter (counter_value + 1)) ]
           [ Vdom.Node.text "count" ]
       ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
