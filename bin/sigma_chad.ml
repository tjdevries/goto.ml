let goto_block _ = (* ... redacted ... *) assert false

let () =
  let i = ref 0 in
  goto_block
    [("loop", (fun () ->
        if !i >= 10 then goto "out" else ();
        printf "%d: Hello, World\n" !i;
        i := !i + 1;
        goto "loop"));
     ("out", (fun () ->
        printf "Done!\n"))]
