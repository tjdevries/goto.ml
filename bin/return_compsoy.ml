(*
int loop() {

loop:
	do_somethiong();
	if (somethiong) { goto done; }
done:
	printf("DONE!");
}
*)

open Goto.GotoFunc

module Filter = struct
  type t = Init | Loop | Done
  let init = Init
  let next = function
    | Init -> Some Loop
    | Loop -> Some Done
    | Done -> None
end

(* alternative syntax, not sure which is nicer *)
(* let labels : Filter.t labels = (module Filter) *)

let filter ~start ~stop predicate =
  let index = ref 0 in

  call (module Filter) @@ fun ~goto ~return -> function
  | Init -> 
            index := start;
            Format.printf "Starting: Searching until: %d@." stop;
  | Loop -> 
            (* Exit the loop if we have reached the end. *)
            if !index >= stop then goto Done;

            (* Check if we have found what we are looking for.
                We even return early here! Very imperative of us *)
            if predicate !index then (
              Format.printf "EARLY RETURN!@.";
              return !index;
            );

            (* "Body" of the loop *)
            Format.printf "Looping: %d@." !index;
            index := !index + 1;

            (* Continue looping *)
            goto Loop
  | Done -> 
            Format.printf "Done!@."

let _ = 
  match filter ~start:2 ~stop:10 (fun i -> i == 5) with
  | Some 5 -> Format.printf "Found 5! @."
  | _ -> failwith "I have failed"
