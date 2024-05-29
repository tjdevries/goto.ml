[@@@warning "-32"]
(* Kind of annoying, but ppx_deriving.enum makes these error *)

let ( let> ) = Option.bind

type 'a goto = 'a -> unit

module type IMPL = sig
  type label [@@deriving enum]
  type ret

  val chunk : label goto -> ret goto -> label -> unit
end

module type Block = sig
  type ret

  val call : unit -> ret option
end

module Make (I : IMPL) : Block with type ret = I.ret = struct
  include I

  exception Goto of label
  exception Return of ret

  let goto label = raise (Goto label)
  let return result = raise (Return result)

  let rec handle label =
    match chunk goto return label with
    | () ->
        let> label = label_of_enum @@ (label_to_enum label + 1) in
        handle label
    | exception Goto label -> handle label
    | exception Return result -> Some result

  let call () : ret option =
    let> label = label_of_enum min_label in
    handle label
end

let call (type a) (module I : IMPL with type ret = a) : a option =
  let module Block = Make (I) in
  Block.call ()
