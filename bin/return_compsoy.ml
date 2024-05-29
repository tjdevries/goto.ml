[@@@ocamlformat "disable"]

open Goto.GotoFunc

module Filter = struct
  type t = Loop | Done [@@deriving enum]
end

let filter start stop predicate =
  let index = ref start in
  let loop =
    make (module Filter) @@ fun goto return -> function
    | Loop -> if !index >= stop then goto Done;
              if predicate !index then return !index;
              Fmt.pr "Looping: %d@." !index;
              incr index;
              goto Loop
    | Done -> Fmt.pr "Done!@.";
  in
  loop ()

let _ = 
  match filter 2 10 (fun i -> i = 8) with
  | Some 8 -> Fmt.pr "Found 8! @."
  | _ -> failwith "I have failed"
