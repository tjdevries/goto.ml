let ( let> ) = Option.bind

(* type labels = Loop | Done [@@deriving enum] *)

module type LABELS = sig
  type t [@@deriving enum]
end

type ('l, 'r) make = ('l -> unit) -> ('r -> unit) -> 'l -> unit
type 'l labels = (module LABELS with type t = 'l)

let make (type l r) (l : l labels) (impl : (l, r) make) : unit -> r option =
  let exception Goto of l in
  let exception Return of r in
  let goto label = raise (Goto label) in
  let return result = raise (Return result) in
  let module L = (val l : LABELS with type t = l) in
  let rec handle label =
    match impl goto return label with
    | () ->
        let> label = L.of_enum @@ (L.to_enum label + 1) in
        handle label
    | exception Goto label -> handle label
    | exception Return result -> Some result
  in
  let call () =
    let> label = L.of_enum L.min in
    handle label
  in
  call
