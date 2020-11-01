open! Core_kernel
open! Bonsai_web
open Bonsai.Let_syntax
module W = Vdom_input_widgets

module User = struct
  module Id = Int

  type t =
    { name : string
    ; surname : string
    }
  [@@deriving sexp, equal]

  let to_string { name; surname } = sprintf "%s, %s" surname name
end

module Model = struct
  type t =
    { users : User.t User.Id.Map.t
    ; last_id : User.Id.t
    }
  [@@deriving sexp, equal]

  let empty = { users = User.Id.Map.empty; last_id = 0 }

  let add user { users; last_id } =
    let id = last_id + 1 in
    { last_id = id; users = User.Id.Map.add_exn users ~key:id ~data:user }
  ;;

  let default =
    empty
    |> add { User.name = "Hans"; surname = "Emil" }
    |> add { User.name = "Max"; surname = "Mustermann" }
    |> add { User.name = "Roman"; surname = "Tisch" }
  ;;
end

module Action = struct
  type t =
    | Create of User.t
    | Update of
        { id : User.Id.t
        ; user : User.t
        }
    | Delete of User.Id.t
  [@@deriving sexp]
end

let apply_action ~inject:_ ~schedule_event:_ model = function
  | Action.Create user -> Model.add user model
  | Update { id; user } ->
    let users = Map.set model.Model.users ~key:id ~data:user in
    { model with users }
  | Delete id ->
    let users = Map.remove model.Model.users id in
    { model with users }
;;

let isolated_model
    (type a cmp)
    (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
    value
    computation
  =
  let value =
    let%map value = value in
    Map.singleton (module M) value ()
  in
  let%sub results = Bonsai.assoc (module M) value ~f:(fun _ _ -> computation) in
  return (Bonsai.Value.map results ~f:(fun r -> r |> Map.data |> List.hd_exn))
;;

module User_id_option = struct
  module T = struct
    type t = User.Id.t option [@@deriving sexp, compare]
  end

  include T
  include Comparator.Make (T)
end

let not_empty_or_whitespace = Fn.non @@ String.for_all ~f:Char.is_whitespace

let user_editor ~for_ =
  let%sub name = Bonsai.state_opt [%here] (module String) in
  let%sub surname = Bonsai.state_opt [%here] (module String) in
  return
  @@ let%map name, set_name = name
     and surname, set_surname = surname
     and for_ = for_ in
     let name =
       match name, for_ with
       | None, Some { User.name; _ } -> Some name
       | Some name, _ when not_empty_or_whitespace name -> Some name
       | _ -> None
     in
     let surname =
       match surname, for_ with
       | None, Some { User.surname; _ } -> Some surname
       | Some surname, _ when not_empty_or_whitespace surname -> Some surname
       | _ -> None
     in
     let view =
       Vdom.Node.div
         []
         [ W.Entry.text ~placeholder:"name" ~value:name ~on_input:set_name ()
         ; W.Entry.text ~placeholder:"surname" ~value:surname ~on_input:set_surname ()
         ]
     in
     let user =
       match name, surname with
       | Some name, Some surname -> Some { User.name; surname }
       | _ -> None
     in
     let clear = Ui_event.Many [ set_name None; set_surname None ] in
     view, user, clear
;;

let component =
  let%sub users =
    Bonsai.state_machine0
      [%here]
      (module Model)
      (module Action)
      ~default_model:Model.default
      ~apply_action
  in
  let%sub selected = Bonsai.state_opt [%here] (module User.Id) in
  let set_selected = Bonsai.Value.map selected ~f:Tuple2.get2 in
  let selected = Bonsai.Value.map selected ~f:Tuple2.get1 in
  let selected_user =
    let%map selected = selected
    and { Model.users; _ }, _ = users in
    let%bind.Option selected = selected in
    Map.find users selected
  in
  let%sub filter = Bonsai.state [%here] (module String) ~default_model:"" in
  let%sub user_editor =
    isolated_model (module User_id_option) selected (user_editor ~for_:selected_user)
  in
  return
  @@ let%map selected = selected
     and set_selected = set_selected
     and user_store, inject_users = users
     and filter, set_filter = filter
     and user_editor_view, user_to_edit, clear = user_editor in
     let users =
       Map.filter user_store.Model.users ~f:(fun { User.surname; _ } ->
           String.Caseless.is_prefix surname ~prefix:filter)
     in
     let module User_id = struct
       type t = User.Id.t [@@deriving equal]

       let to_string t =
         match Map.find users t with
         | None -> ""
         | Some user -> User.to_string user
       ;;
     end
     in
     let filter =
       W.Entry.text
         ~placeholder:"surname prefix filter"
         ~value:(Some filter)
         ~on_input:(function
           | Some s -> set_filter s
           | None -> set_filter "")
         ()
     in
     let dropdown =
       W.Dropdown.of_values_opt
         (module User_id)
         (Map.keys users)
         ~extra_attrs:
           [ Vdom.Attr.style (Css_gen.overflow `Auto)
           ; Vdom.Attr.create "size" (Int.to_string (1 + Map.length users))
           ]
         ~selected
         ~on_change:set_selected
     in
     let create_button =
       let validation =
         match selected, user_to_edit with
         | None, Some user -> Ok user
         | _ -> Error "blah"
       in
       W.Button.with_validation "create" ~validation ~on_click:(fun user ->
           Ui_event.Many [ clear; inject_users (Action.Create user) ])
     in
     let update_button =
       let validation =
         match selected, user_to_edit with
         | Some id, Some user -> Ok (id, user)
         | _ -> Error "blah"
       in
       W.Button.with_validation "update" ~validation ~on_click:(fun (id, user) ->
           Ui_event.Many [ clear; inject_users (Action.Update { id; user }) ])
     in
     let delete_button =
       let validation =
         match selected, user_to_edit with
         | Some id, Some _ -> Ok id
         | _ -> Error "blugh"
       in
       W.Button.with_validation "delete" ~validation ~on_click:(fun id ->
           Ui_event.Many [ clear; inject_users (Action.Delete id) ])
     in
     Vdom.Node.div
       []
       [ filter
       ; Vdom.Node.div [] [ dropdown ]
       ; user_editor_view
       ; Vdom.Node.div [] [ create_button; update_button; delete_button ]
       ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
