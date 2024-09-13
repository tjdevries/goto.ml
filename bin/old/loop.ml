open Goto.GotoModule

let do_inner_loop i limit =
  let module InnerLoop = struct
    type labels = [ `Loop | `Done ]
    let default = `Loop
    let next = function
      | `Loop -> Some `Done
      | `Done -> None

    type return = unit

    let index = ref i

    let chunk goto _ = function
      | `Loop ->
          if !index >= limit then goto `Done;
          Format.printf "  Inner Loop: %d@." !index;
          index := !index + 1;
          goto `Loop
      | `Done -> Format.printf "  Inner Loop Done!@."
  end in
  call (module InnerLoop)

let do_loop index limit =
  let index = ref index in
  let module Loop = struct
    type labels = [ `Loop | `Done ]
    let default = `Loop
    let next = function
      | `Loop -> Some `Done
      | `Done -> None

    type return = unit

    let chunk goto return = function
      | `Loop ->
          if !index >= limit then goto `Done;
          if !index == 4 then return ();
          do_inner_loop !index 5 |> ignore;
          incr index;
          Format.printf "Looping: %d@." !index;
          goto `Loop
      | `Done ->
          Format.printf "Quitting Loop: %d!@." !index;
          return ()
  end in
  call (module Loop)

let _ = do_loop 2 10
