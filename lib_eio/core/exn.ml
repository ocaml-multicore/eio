let show_backend_exceptions = ref true

type with_bt = exn * Printexc.raw_backtrace

type err = ..

type context = {
  steps : string list;
}

exception Io of err * context

exception Multiple of (exn * Printexc.raw_backtrace) list  (* Note: the last exception in list is the first one reported *)

type err += Multiple_io of (err * context * Printexc.raw_backtrace) list

exception Cancelled of exn

exception Cancel_hook_failed of exn list

let create err = Io (err, { steps = [] })

let add_context ex fmt =
  fmt |> Fmt.kstr @@ fun msg ->
  match ex with
  | Io (code, t) -> Io (code, {steps = msg :: t.steps})
  | ex -> ex

let reraise_with_context ex bt fmt =
  fmt |> Fmt.kstr @@ fun msg ->
  match ex with
  | Io (code, t) ->
    let context = { steps = msg :: t.steps } in
    Printexc.raise_with_backtrace (Io (code, context)) bt
  | _ ->
    Printexc.raise_with_backtrace ex bt

let err_printers : (Format.formatter -> err -> bool) list ref = ref []

let register_pp fn =
  err_printers := fn :: !err_printers

let break f _ = Format.pp_print_custom_break f
    ~fits:(",", 1, "")
    ~breaks:(",", 2, "")

let pp_err f x =
  let rec aux = function
    | [] -> Fmt.string f "?"
    | pp :: pps -> if not (pp f x) then aux pps
  in
  aux !err_printers

let pp_with_context f (code, context) =
  Fmt.pf f "%a%a" pp_err code
    Fmt.(list ~sep:nop (break ++ string)) (List.rev context.steps)

let pp_with_bt f (code, context, bt) =
  match String.trim (Printexc.raw_backtrace_to_string bt) with
  | "" ->
    Fmt.pf f "- @[<hov>%a@]"
      pp_with_context (code, context)
  | bt ->
    Fmt.pf f "- @[<v>%a@,%a@]"
      pp_with_context (code, context)
      Fmt.lines bt

let pp f = function
  | Io (code, t) ->
    Fmt.pf f "Eio.Io %a%a"
      pp_err code
      Fmt.(list ~sep:nop (break ++ string)) (List.rev t.steps)
  | ex ->
    Fmt.string f (Printexc.to_string ex)

let pp_multiple f exns =
  let pp_with_bt f (ex, bt) =
    match String.trim (Printexc.raw_backtrace_to_string bt) with
    | "" ->
      Fmt.pf f "- @[<v>%a@]" pp ex
    | bt ->
      Fmt.pf f "- @[<v>%a@,%a@]"
        pp ex
        Fmt.lines bt
  in
  Fmt.pf f "@[<v>Multiple exceptions:@,%a@]"
    (Fmt.(list ~sep:cut) pp_with_bt) (List.rev exns)

let () =
  Printexc.register_printer @@ function
  | Io _ as ex -> Some (Fmt.str "@[<v>%a@]" pp ex)
  | Multiple exns -> Some (Fmt.str "%a" pp_multiple exns)
  | Cancel_hook_failed exns -> Some ("During cancellation:\n" ^ String.concat "\nand\n" (List.map Printexc.to_string exns))
  | Cancelled ex -> Some ("Cancelled: " ^ Printexc.to_string ex)
  | _ -> None

let combine e1 e2 =
  if fst e1 == fst e2 then e1
  else match e1, e2 with
    | (Cancelled _, _), e
    | e, (Cancelled _, _) -> e  (* Don't need to report a cancelled exception if we have something better *)
    | (Io (c1, t1), bt1), (Io (c2, t2), bt2) -> create (Multiple_io [(c1, t1, bt1); (c2, t2, bt2)]), Printexc.get_callstack 0
    | (Multiple exs, bt1), e2 -> Multiple (e2 :: exs), bt1
    | e1, e2 -> Multiple [e2; e1], Printexc.get_callstack 0

module Backend = struct
  type t = ..

  let show = ref true

  let printers : (Format.formatter -> t -> bool) list ref = ref []

  let register_pp fn =
    printers := fn :: !printers

  let pp f x =
    if !show then (
      let rec aux = function
        | [] -> Fmt.string f "?"
        | pp :: pps -> if not (pp f x) then aux pps
      in
      aux !printers
    ) else Fmt.string f "_"
end

type err += X of Backend.t

let () =
  register_pp (fun f -> function
      | Multiple_io errs -> Fmt.pf f "Multiple_io@\n%a" (Fmt.(list ~sep:cut) pp_with_bt) errs; true
      | X ex -> Backend.pp f ex; true
      | _ -> false
    )
