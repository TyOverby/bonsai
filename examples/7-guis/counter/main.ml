open! Core_kernel
open! Bonsai_web
open Bonsai.Let_syntax
module W = Vdom_input_widgets

let component  = 
  let%sub temp_c = Bonsai.state [%here] (module Float) ~default_model:0.0 in 
  return @@
  let%map celsius, set_celsius = temp_c in 
  let f = W.Entry.text "" in
  Vdom.Node.div [] [
    Vdom.Node.textf "%d" counter_value;
    Vdom.Node.button [Vdom.Attr.on_click (fun _ -> set_counter (counter_value + 1))] [Vdom.Node.text "count"]
  ]

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
