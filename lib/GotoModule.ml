[@@@warning "-32"]
(* Kind of annoying, but ppx_deriving.enum makes these error *)

let ( let> ) = Option.bind

type 'a goto = 'a -> unit

module type IMPL = sig
  type labels [@@deriving enum]
  type return

  val chunk : labels goto -> return goto -> labels -> unit
end

module type Block = sig
  type return

  val call : unit -> return option
end

module Make (I : IMPL) : Block with type return = I.return = struct
  include I

  exception Goto of labels
  exception Return of return

  let goto label = raise (Goto label)
  let return result = raise (Return result)

  let rec handle label =
    match chunk goto return label with
    | () ->
        let> label = labels_of_enum @@ (labels_to_enum label + 1) in
        handle label
    | exception Goto label -> handle label
    | exception Return result -> Some result

  let call () : return option =
    let> label = labels_of_enum min_labels in
    handle label
end

let call (type a) (module I : IMPL with type return = a) : a option =
  let module Block = Make (I) in
  Block.call ()
