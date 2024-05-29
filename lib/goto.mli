type 'a goto = ('a -> unit)

module type IMPL = sig
  type label [@@deriving enum]
  type ret

  val chunk : label goto -> ret goto -> label -> unit
end

module type Block = sig
  type ret

  val call : unit -> ret option
end

val call : (module IMPL with type ret = 'a) -> 'a option
