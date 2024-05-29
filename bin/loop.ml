open Goto

let do_inner_loop i limit =
  let module InnerLoop = struct
    type label = [ `Loop | `Done ] [@@deriving enum]
    type ret = unit

    let index = ref i

    let chunk goto _ = function
      | `Loop ->
          if !index >= limit then goto `Done;
          Fmt.pr "  Inner Loop: %d@." !index;
          index := !index + 1;
          goto `Loop
      | `Done -> Fmt.pr "  Inner Loop Done!@."
  end in
  call (module InnerLoop)

let do_loop index limit =
  let index = ref index in
  let module Loop = struct
    type label = [ `Loop | `Done ] [@@deriving enum]
    type ret = unit

    let chunk goto return = function
      | `Loop ->
          if !index >= limit then goto `Done;
          if !index == 4 then return ();
          do_inner_loop !index 5 |> ignore;
          incr index;
          Fmt.pr "Looping: %d@." !index;
          goto `Loop
      | `Done ->
          Fmt.pr "Quitting Loop: %d!@." !index;
          return ()
  end in
  call (module Loop)

let _ = do_loop 2 10
