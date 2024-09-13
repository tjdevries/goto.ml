open Goto.GotoModule

[@@@ocamlformat "disable"]
let find_line path predicate =
  let result = ref None in
  let handle = ref None in

  let module ForLines = struct
    type labels = Start | Cleanup | Done [@@deriving enum]
    type return = string

    let chunk goto return = function
| Start   -> Fmt.pr "=> opening %s@." path;
             let h = open_in path in
             handle := Some h;
             begin try
               while true do
                 let line = input_line h in
                 if predicate line then (
                   result := Some line;
                   goto Cleanup)
               done
               with _ -> ()
             end
| Cleanup -> Fmt.pr "=> cleaning up@.";
             Option.iter
               (fun h ->
                 close_in h;
                 handle := None)
               !handle
| Done    -> Option.iter return !result
  end in
  call (module ForLines)
[@@@ocamlformat "enable"]

(* Find a line in a file. *)
let _ =
  let path = "README.md" in
  let msg = find_line path (fun line -> line = "Yup :)") in
  match msg with
  | Some line -> Fmt.pr "==== %s => %s@." path line
  | None -> failwith "not possible"

(* This is an example of handling `Cleanup even if predicate throws an exception. *)
let _ =
  let path = "README.md" in
  let msg = find_line path (fun _ -> failwith "OH NO") in
  match msg with
  | Some _ -> failwith "not possible"
  | None -> Fmt.pr "Should not find a line@."
