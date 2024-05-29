type 'a goto = ('a -> unit)

module type IMPL = sig
  type labels [@@deriving enum]
  type return

  val chunk : labels goto -> return goto -> labels -> unit
end

module type Block = sig
  type return

  val call : unit -> return option
end

val call : (module IMPL with type return = 'a) -> 'a option
