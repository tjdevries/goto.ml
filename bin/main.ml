open Effect
open Effect.Deep

let () = Fmt.pr "@.@.Hello, World!@."

type 'a gotoer = ('a -> unit) -> unit

module type GOTO = sig
  type label

  val get_handler : label -> label gotoer
end

module MakeGoto (G : GOTO) = struct
  type _ eff += Goto : G.label -> unit eff

  let goto (label : G.label) : unit = perform (Goto label)

  let rec handle f  =
    match f goto with
    | () -> ()
    | effect (Goto label), _ -> 
        let handler = G.get_handler label in
        handle handler

  let start label = 
    handle (fun goto -> goto label)
end

module Block = struct
  type label = [`loop of int | `out]

  let shared = ref 0

  let loop goto n =
    if n >= 10 then goto `out;
    shared := !shared + n;
    Fmt.pr "Looping: %d / %d@." n !shared;
    goto (`loop (n + 1))

  let out _ () =
    Fmt.pr "Out -> %d!@." !shared

  let get_handler label =
    match label with
    | `loop x -> (fun goto -> loop goto x)
    | `out -> (fun goto -> out goto ())
end

module Complete = MakeGoto (Block)

let _ = Complete.start (`loop 1)
