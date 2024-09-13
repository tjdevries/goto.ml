let ( let> ) = Option.bind

type 'a goto = 'a -> unit

module type IMPL = sig
  type labels
  type return

  val default : labels
  val next : labels -> labels option
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
        let> label = next label in
        handle label
    | exception Goto label -> handle label
    | exception Return result -> Some result

  let call () : return option =
    let> label = Some default in
    handle label
end

let call (type a) (module I : IMPL with type return = a) : a option =
  let module Block = Make (I) in
  Block.call ()
