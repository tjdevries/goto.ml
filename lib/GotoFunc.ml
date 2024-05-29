let ( let> ) = Option.bind

module type LABELS = sig
  type t [@@deriving enum]
end

type ('l, 'r) t = ('l -> unit) -> ('r -> unit) -> 'l -> unit
type 'l labels = (module LABELS with type t = 'l)

let make (type l r) ((module L) : l labels) (impl : (l, r) t) : l -> r option =
  let open struct
    type exn += Goto of l | Return of r

    let goto label = raise (Goto label)
    let return result = raise (Return result)
  end in
  let rec handle label =
    match impl goto return label with
    | () ->
        let> label = L.of_enum @@ (L.to_enum label + 1) in
        handle label
    | exception Goto label -> handle label
    | exception Return result -> Some result
  in
  handle

let call (type l) (l : l labels) impl =
  let (module L) = l in
  let handle = make l impl in
  let> label = L.of_enum L.min in
  handle label
