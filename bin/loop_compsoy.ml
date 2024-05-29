[@@@ocamlformat "disable"]

open Goto.GotoFunc

module Loop = struct
  type t = Loop | Done [@@deriving enum]
end

let do_loop start stop =
  let index = ref start in
  call (module Loop) @@ fun goto return -> function
  | Loop -> if !index >= stop then goto Done;
            Fmt.pr "Looping: %d@." !index;
            incr index;
            goto Loop
  | Done -> Fmt.pr "Done!@.";
            return ()

let _ = do_loop 2 10
