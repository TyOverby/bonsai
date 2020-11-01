open! Core_kernel
open! Bonsai_web
open Bonsai.Let_syntax
module W = Vdom_input_widgets

module Kind = struct 
  type t = One_way | Two_way [@@deriving sexp, equal, enumerate]
  let to_string = function 
    | One_way -> "one-way flight"
    | Two_way -> "round-trip flight"
end

let is_ok_date text = 
  try 
    let _: Date.t = Date.of_string text in 
    true
  with 
    | _ -> false

let attrs_for_date_string ~disabled text = 
  let pink_background = Vdom.Attr.style (Css_gen.background_color (`Name "pink")) in
  if is_ok_date text || disabled then
    []  else
      [pink_background] ;;

  let textbox_for_date ~value ~set_value ~disabled = 
    W.Entry.text ~disabled ~extra_attrs:(attrs_for_date_string ~disabled value) ~value:(Some value) ~on_input:(
      Option.value_map ~default:(set_value "") ~f:(set_value)) () ;;


let component  = 
  let%sub which = Bonsai.state [%here] (module Kind) ~default_model:Kind.One_way in 
  let%sub outgoing_state = Bonsai.state [%here] (module String) ~default_model:"" in 
  let%sub incoming_state = Bonsai.state [%here] (module String) ~default_model:"" in 
  let%sub last_flight = Bonsai.state [%here] (module String) ~default_model:"" in 
  return @@
  let%map 
  outgoing, set_outgoing = outgoing_state and
  incoming, set_incoming = incoming_state and
  which , set_which = which and
  last_flight, set_last_flight = last_flight
  in 
  let dropdown = W.Dropdown.of_enum (module Kind) ~selected:which ~on_change:set_which in

  let outgoing_view = textbox_for_date ~value:outgoing ~set_value:set_outgoing ~disabled:false in
  let incoming_view = 
    let disabled = not (Kind.equal which Kind.Two_way) in
    textbox_for_date ~value:incoming ~set_value:set_incoming ~disabled in 

  let booking_message = match which with 
      | Kind.One_way -> Option.some_if (is_ok_date outgoing) (sprintf "You have booked a one-way flight on %s" outgoing)
      | Kind.Two_way -> Option.some_if (is_ok_date outgoing && is_ok_date outgoing) (sprintf "You have booked a round-trip flight on %s and %s" outgoing incoming) 
  in
  let booking_message = Result.of_option booking_message ~error:"invalid dates present" in
  let submit_button = W.Button.with_validation "Book" ~validation:booking_message ~on_click:set_last_flight in

  Vdom.Node.div [] [
    Vdom.Node.div [] [
    dropdown
    ] ;
    Vdom.Node.div [] [outgoing_view] ;
    Vdom.Node.div [] [incoming_view] ;
    Vdom.Node.div [] [submit_button];
    Vdom.Node.div [] [Vdom.Node.text last_flight]
    ]

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
