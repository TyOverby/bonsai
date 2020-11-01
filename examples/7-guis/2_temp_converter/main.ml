open! Core_kernel
open! Bonsai_web
open Bonsai.Let_syntax
module W = Vdom_input_widgets

let ( >> ) f g x = g (f x)

module Float = struct
  include Float

  let to_string f = to_string f ^ "0"
end

let to_farenheit c = (c *. (9.0 /. 5.0)) +. 32.0
let to_celsius f = (f -. 32.0) *. (5.0 /. 9.0)

let textbox ~value ~setter =
  W.Entry.number
    (module Float)
    ~value:(Some value)
    ~step:1.0
    ~on_input:(Option.value_map ~default:Ui_event.Ignore ~f:(fun f -> setter f))
;;

let component =
  let%sub temp_c = Bonsai.state [%here] (module Float) ~default_model:0.0 in
  return
  @@ let%map celsius, set_celsius = temp_c in
     let f = textbox ~value:(to_farenheit celsius) ~setter:(to_celsius >> set_celsius) in
     let c = textbox ~value:celsius ~setter:set_celsius in
     Vdom.Node.div [] [ f; Vdom.Node.text "farenheit = "; c; Vdom.Node.text "celsius" ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;