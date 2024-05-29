[@@@ocamlformat "disable"]

open Goto.GotoFunc

module Filter = struct
  type t = Loop | Done [@@deriving enum]
end

(* alternative syntax, not sure which is nicer *)
(* let labels : Filter.t labels = (module Filter) *)

let filter start stop predicate =
  let index = ref start in
  call (module Filter) @@ fun goto return -> function
  | Loop -> if !index >= stop then goto Done;
            if predicate !index then return !index;
            Fmt.pr "Looping: %d@." !index;
            incr index;
            goto Loop
  | Done -> Fmt.pr "Done!@."

let _ = 
  match filter 2 10 (fun i -> i = 8) with
  | Some 8 -> Fmt.pr "Found 8! @."
  | _ -> failwith "I have failed"
