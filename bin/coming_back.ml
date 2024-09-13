open Effect
open Effect.Deep

module type GOTO = sig
  type label
  type value

  val chunk : (label -> value) -> (label -> unit) -> label -> value
end

module MakeGoto (G : GOTO) = struct
  type label = G.label
  type value = G.value

  type _ eff += 
    | Goto :  label -> value eff
    | Comeback : label -> unit eff

  let goto label = 
    perform (Goto label)

  let comeback label =
    perform (Comeback label)

  let rec handle (f : unit -> value) : value =
    match f () with
    | result -> result
    | effect (Goto label), _ ->
        handle (fun () -> G.chunk goto comeback label)
    | effect (Comeback label), k ->
        handle (fun () -> 
          let _ = G.chunk goto comeback label in
          continue k ())

  let start (label : label) : value = 
    handle (fun () -> goto label)
end

module T = (struct
  type value = string
  type label =
    | Read of string
    | Defer of (unit -> unit)
    | Reading of in_channel
    | Cleanup 

  let deferred = ref []
  let lines = ref ""

  let max_defers = 5

  let chunk goto comeback =
    let defer f = comeback (Defer f) |> ignore in function
    | Read path -> 
        Format.printf "1. Read %s@." path;
        let handle = open_in path in
        defer (fun () -> 
          Format.printf "6. deferred: Closing Handle!@.";
          close_in handle);

        goto (Reading handle)
    | Reading handle ->
        Format.printf "2. About to read handle!@.";
        defer (fun () -> Format.printf "5. deferred: And another one!@.");

        Format.printf "3. Reading handle!@.";
        let n = in_channel_length handle in
        lines := really_input_string handle n;
        goto Cleanup
    | Defer f ->
        if List.length !deferred >= max_defers then (
          Format.printf "\n==== TOO MANY DEFERS I QUIT ====\n\n";
          goto Cleanup |> ignore
        );

        Format.printf "  -> appending defer@.";
        deferred := f ::!deferred;
        !lines
    | Cleanup ->
        Format.printf "4. Cleanup!@.";
        List.iter (fun f -> f ()) !deferred;
        !lines

end)
  
let _ = 
  let open MakeGoto(T) in
  Format.printf "@.==> Starting Now <==@.";
  let msg = start (Read "README.md") in
  Format.printf "7. Done with reading: %d characters@." (String.length msg)
