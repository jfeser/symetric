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

    let to_sequence x = Set.to_sequence !x

    let random_element_exn x =
      Option.value_exn (Set.nth !x (Random.int @@ length x))
  end

  let eval = Lang.Conc.eval

  exception Done

  let hamming (params : (Cad.Bench.t, _) Params.t) c c' =
    let ct = ref 0 in
    for x = 0 to params.bench.input.xmax - 1 do
      for y = 0 to params.bench.input.ymax - 1 do
        let p =
          Vector2.{ x = Float.of_int x +. 0.5; y = Float.of_int y +. 0.5 }
        in
        ct :=
          !ct + if Bool.(Cad.Conc.getp c p = Cad.Conc.getp c' p) then 0 else 1
      done
    done;
    !ct

  type stats = {
    mutable n_states : int;
    mutable n_iters : int;
    mutable best_dist : int;
    mutable solved : bool;
  }

  let create_stats () =
    { n_states = 0; n_iters = 0; best_dist = 0; solved = false }

  let synth (params : _ Params.t) stats =
    let ops = Bench.ops params.bench
    and output = Bench.output params.bench
    and states = State_set.create () in
    let non_nil_ops =
      List.filter ops ~f:(fun op -> Op.arity op > 0)
      |> List.sort ~compare:(fun o o' -> compare (Op.arity o) (Op.arity o'))
    in

    let op_sets = Hashtbl.create (module Op) in
    List.iter ops ~f:(fun op ->
        let sets = Array.init (Op.arity op) ~f:(fun _ -> State_set.create ()) in
        Hashtbl.set op_sets ~key:op ~data:sets);

    let select_args (dist : _ -> _ -> int) seen all =
      let sample =
        List.init
          (Int.min (State_set.length all) 10)
          ~f:(fun _ -> State_set.random_element_exn all)
      in
      let seen_sample = List.take (List.permute seen) 10 in
      let sample, _ =
        Option.value_exn
          (List.map sample ~f:(fun v ->
               let min_dist =
                 List.map seen_sample ~f:(dist v)
                 |> List.min_elt ~compare
                 |> Option.value ~default:Int.max_value
               in
               (v, min_dist))
          |> List.max_elt ~compare:(fun (_, d) (_, d') -> compare d d'))
      in
      sample
    in

    let fill () =
      let best = ref None in
      while true do
        stats.n_iters <- stats.n_iters + 1;

        let op = List.random_element_exn non_nil_ops in
        let op_sets = Hashtbl.find_exn op_sets op in
        let args =
          List.init (Op.arity op) ~f:(fun i ->
              let arg =
                select_args (hamming params)
                  (State_set.to_list op_sets.(i))
                  states
              in
              State_set.add op_sets.(i) arg;
              arg)
        in

        let out = eval params op args in
        (match !best with
        | None ->
            stats.best_dist <- hamming params output out;
            best := Some out
        | Some _ ->
            let dist = hamming params output out in
            if stats.best_dist > dist then (
              best := Some out;
              stats.best_dist <- dist));

        if stats.n_iters mod 10 = 0 then
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
