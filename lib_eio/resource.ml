type ('t, 'impl, 'tags) pi = ..
type _ binding = H : ('t, 'impl, 'tags) pi * 'impl -> 't binding
type 't ops = 't binding array
type ('t, 'tags) handler = 't ops
type -'a t = T : ('t * 't ops) -> 'a t

let not_supported () = failwith "Operation not supported!"

let handler = Array.of_list
let bindings = Array.to_list

let get : 't ops -> ('t, 'impl, 'tags) pi -> 'impl = fun ops op ->
  let rec aux i =
    if i = Array.length ops then not_supported ();
    let H (k, v) = ops.(i) in
    if Obj.repr k == Obj.repr op then Obj.magic v
    else aux (i + 1)
  in
  aux 0

let get_opt : 't ops -> ('t, 'impl, 'tags) pi -> 'impl option = fun ops op ->
  let rec aux i =
    if i = Array.length ops then None
    else (
      let H (k, v) = ops.(i) in
      if Obj.repr k == Obj.repr op then Some (Obj.magic v)
      else aux (i + 1)
    )
  in
  aux 0

type close_ty = [`Close]
type (_, _, _) pi += Close : ('t, 't -> unit, [> close_ty]) pi

let close (T (t, ops)) = get ops Close t
