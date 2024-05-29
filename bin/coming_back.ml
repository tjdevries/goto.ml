open Effect
open Effect.Deep

module type GOTO = sig
  type label
  type ret

  val chunk : ((bool * label) -> ret) -> label -> ret
end

module MakeGoto (G : GOTO) = struct
  type _ eff += Goto : (bool * G.label) -> G.ret eff

  let goto label = perform (Goto label)

  let rec handle f : G.ret =
    match f goto with
    | result -> result
    | effect (Goto (c, label)), k -> 
        let result = handle (fun goto -> G.chunk goto label ) in
        if c then continue k result else result

  let start label : G.ret = 
    handle (fun goto -> goto (false, label))
end

module Defer = MakeGoto(struct
  type ret = string
  type label = [ 
    | `read of string
    | `defer of (unit -> unit)
    | `reading of in_channel
    | `cleanup ]

  let deferred = ref []
  let lines = ref ""

  let defer goto f = 
    goto (true, `defer f) |> ignore

  let chunk goto label : ret = match label with
    | `read path -> 
        let handle = open_in path in

        defer goto (fun () -> 
          Fmt.pr "Closing Handle!@.";
          close_in handle);

        goto (true, `reading handle)
    | `reading handle ->
        defer goto (fun () -> Fmt.pr "And another one!@.");

        let n = in_channel_length handle in
        lines := really_input_string handle n;
        goto (false, `cleanup)
    | `defer f ->
        deferred := f ::!deferred;
        !lines
    | `cleanup ->
        List.iter (fun f -> f ()) !deferred;
        !lines

end)

let _ = 
  Fmt.pr "==> Starting Now <==@.";
  let msg = Defer.start (`read "README.md") in
  Fmt.pr "Done with:@.%s@." msg
