let ( let> ) = Option.bind

module type LABELS = sig
  type t

  val init : t
  val next : t -> t option
end

type ('l, 'r) t = goto:('l -> unit) -> return:('r -> unit) -> 'l -> unit
type 'l labels = (module LABELS with type t = 'l)

let make (type l r) ((module L) : l labels) (impl : (l, r) t) : l -> r option =
  let open struct
    type exn += 
      | Goto of l
      | Return of r

    let goto label = raise (Goto label)
    let return result = raise (Return result)
  end in
  let rec handle label =
    match impl ~goto ~return label with
    | () ->
        let> label = L.next label in
        handle label
    | exception Goto label -> handle label
    | exception Return result -> Some result
  in
  handle

let call (type l) (l : l labels) impl =
  let (module L) = l in
  let handle = make l impl in
  let> label = Some L.init in
  handle label
