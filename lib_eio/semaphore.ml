type t = {
  id : Ctf.id;
  state : Sem_state.t;
}

let make n =
  let id = Ctf.mint_id () in
  Ctf.note_created id Ctf.Semaphore;
  {
    id;
    state = Sem_state.create n;
  }

let release t =
  Ctf.note_signal t.id;
  Sem_state.release t.state

let acquire t =
  if not (Sem_state.acquire t.state) then (
    (* No free resources.
       We must wait until one of the existing users increments the counter and resumes us.
       It's OK if they resume before we suspend; we'll just pick up the token they left. *)
    Suspend.enter_unchecked (fun ctx enqueue ->
        match Sem_state.suspend t.state (fun () -> enqueue (Ok ())) with
        | None -> ()   (* Already resumed *)
        | Some request ->
          Ctf.note_try_read t.id;
          match Fiber_context.get_error ctx with
          | Some ex ->
            if Sem_state.cancel request then enqueue (Error ex);
            (* else already resumed *)
          | None ->
            Fiber_context.set_cancel_fn ctx (fun ex ->
                if Sem_state.cancel request then enqueue (Error ex)
                (* else already resumed *)
              )
      )
  );
  Ctf.note_read t.id

let get_value t =
  max 0 (Atomic.get t.state.state)
