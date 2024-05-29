open Effect

let () = Fmt.pr "@.@.Hello, World!@."

module type GOTO = sig
  type label
  type ret

  val chunk : (label -> ret) -> label -> ret
end

module MakeGoto (G : GOTO) = struct
  type _ eff += Goto : G.label -> G.ret eff

  let goto label = perform (Goto label)

  let rec handle f : G.ret =
    match f goto with
    | result -> result
    | effect (Goto label), _ -> 
        handle (fun goto -> G.chunk goto label )

  let start label : G.ret = 
    handle (fun goto -> goto label)
end

module Loop = MakeGoto(struct
  type label = [`loop of int | `out of string]
  type ret = unit

  let shared = ref 0

  let chunk goto = function
    | `loop limit ->
        if !shared >= limit then goto (`out "KEKW");
        shared := !shared + 1;
        Fmt.pr "Looping: %d@." !shared;
        goto (`loop limit)

    | `out msg ->
        Fmt.pr "Quitting Loop: %d -> %s!@." !shared msg
end)

let _ = Loop.start (`loop 5)

module Cleanup = MakeGoto(struct
  type ret = string
  type label = [ 
    | `start of bool
    | `cleanup of string
    | `out of string ]

  let chunk goto label : ret = match label with
    | `start success ->
        Fmt.pr "=> start@.";
        Fmt.pr "Opening Handle...@.";
        if not success then goto (`cleanup "nonono") |> ignore;
        Fmt.pr "... doing some work with the handle...@.";
        goto (`cleanup "yayaya");
    | `cleanup msg ->
        Fmt.pr "=> cleanup@.";
        goto (`out msg)
    | `out msg ->
        Fmt.pr "=> out@.";
        Fmt.pr "Done %s@." msg;
        msg

end)

let _ = 
  Fmt.pr "==> TRUE@.";
  let msg = Cleanup.start (`start true) in
  Fmt.pr "Done with %s@." msg

let _ = 
  Fmt.pr "==> FALSE@.";
  let msg = Cleanup.start (`start false) in
  Fmt.pr "Done with %s@." msg
