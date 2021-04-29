module Make
    (Lang : Lang_intf.S
              with type Conc.t = Cad.Conc.t
               and type bench = Cad.Bench.t) =
struct
  open Lang

  module State_set = struct
    type t = Set.M(Lang.Conc).t ref

    let create () = ref (Set.empty (module Lang.Conc))

    let length x = Set.length !x

    let add s x = s := Set.add !s x

    let to_list x = Set.to_list !x

    let random_element_exn x =
      Option.value_exn (Set.nth !x (Random.int @@ length x))
  end

  let eval = Lang.Conc.eval

  exception Done

  let hamming = Cad.Conc.hamming

  type stats = {
    mutable n_states : int;
    mutable n_iters : int;
    mutable best_dist : int;
    mutable solved : bool;
  }

  let create_stats () =
    { n_states = 0; n_iters = 0; best_dist = 0; solved = false }

  let synth (params : _ Params.t) stats =
    let ops = Bench.ops params.bench and states = State_set.create () in
    let non_nil_ops =
      List.filter ops ~f:(fun op -> Op.arity op > 0)
      |> List.sort ~compare:(fun o o' -> compare (Op.arity o) (Op.arity o'))
    in

    let fill () =
      let best = ref None in
      while true do
        stats.n_iters <- stats.n_iters + 1;

        let op = List.random_element_exn non_nil_ops in
        let args =
          List.init (Op.arity op) ~f:(fun _ ->
              State_set.random_element_exn states)
        in
        let out = eval params op args in

        (match !best with
        | None ->
            stats.best_dist <- hamming params out;
            best := Some out
        | Some _ ->
            let dist = hamming params out in
            if dist < stats.best_dist then (
              stats.best_dist <- dist;
              best := Some out));

        if stats.n_iters mod 1000 = 0 then
          eprint_s
            [%message (stats.best_dist : int) (State_set.length states : int)];

        if [%compare.equal: Lang.Conc.t] out (Bench.output params.bench) then (
          stats.solved <- true;
          raise Done);

        State_set.add states out;
        stats.n_states <- State_set.length states
      done
    in

    try
      List.iter ops ~f:(fun op ->
          if Op.arity op = 0 then
            let state = eval params op [] in
            State_set.add states state);

      fill ()
    with Done -> ()
end
