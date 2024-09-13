open Goto.GotoFunc

module Loop = struct
  type t =
    | Loop
    | Done

  let init = Loop
  let next = function
    | Loop -> Some Done
    | Done -> None
end

let do_loop start stop =
  let index = ref start in
  call (module Loop) @@ fun ~goto ~return -> function
  | Loop -> if !index >= stop then goto Donee;
            Format.printf "Looping: %d@." !index;
            incr index;
            goto Loop
  | Done -> Format.printf "Done!@.";
            return ()

let _ = do_loop 2 10
