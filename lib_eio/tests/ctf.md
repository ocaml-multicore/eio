# Test unique ID generation


```ocaml
# #require "eio";;
# for _ = 1 to 5 do
    Printf.printf "%d\n%!" (Eio.Private.Ctf.mint_id () :> int)
  done;;
1
2
3
4
5
- : unit = ()
```

A new domain gets a new chunk:

```ocaml
# Domain.join @@ Domain.spawn
    (fun () ->
      for _ = 1 to 5 do
        Printf.printf "%d\n%!" (Eio.Private.Ctf.mint_id () :> int)
      done);;
1024
1025
1026
1027
1028
- : unit = ()
```

When the original domain exhausts its chunk, it jumps to the next free chunk:

```ocaml
# for _ = 1 to 1024 - 9 do
    Eio.Private.Ctf.mint_id () |> ignore
  done;;
- : unit = ()

# for _ = 1 to 5 do
    Printf.printf "%d\n%!" (Eio.Private.Ctf.mint_id () :> int)
  done;;
1021
1022
1023
2048
2049
- : unit = ()
```
