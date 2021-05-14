module Make
    (Lang : Lang_intf.S
              with type Value.t = Cad.Value.t
               and type bench = Cad.Bench.t) =
struct
  open Lang

  module State_set = struct
    type t = Set.M(Value).t ref

    let create () = ref (Set.empty (module Value))

    let length x = Set.length !x

    let add s x = s := Set.add !s x

    let to_list x = Set.to_list !x

    let to_array x = Set.to_array !x

    let random_element_exn x =
      Option.value_exn (Set.nth !x (Random.int @@ length x))
  end

  let eval = Value.eval

  exception Done

  let hamming (params : (Cad.Bench.t, _) Params.t) c c' =
    let ct = ref 0 in
    for x = 0 to params.bench.input.xmax - 1 do
      for y = 0 to params.bench.input.ymax - 1 do
        let p =
          Vector2.{ x = Float.of_int x +. 0.5; y = Float.of_int y +. 0.5 }
        in
        ct :=
          !ct + if Bool.(Cad.Value.getp c p = Cad.Value.getp c' p) then 0 else 1
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
    and expected = Bench.output params.bench
    and states = State_set.create () in
    let non_nil_ops =
      List.filter ops ~f:(fun op -> Op.arity op > 0)
      |> List.sort ~compare:(fun o o' -> compare (Op.arity o) (Op.arity o'))
    in

    let module Point = struct
      type t = Value.t

      let sexp_of_t _ = Sexp.Atom "p"

      let t_of_sexp _ = failwith ""

      let dist x x' = Float.of_int @@ hamming params x x'
    end in
    let module Vpt = Vp_tree.Make (Point) in
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
            stats.best_dist <- hamming params expected out;
            best := Some out
        | Some _ ->
            let dist = hamming params expected out in
            if dist < stats.best_dist then (
              stats.best_dist <- dist;
              best := Some out));

        if stats.n_iters mod 1000 = 0 then
          eprint_s
            [%message (stats.best_dist : int) (State_set.length states : int)];

        if [%compare.equal: Value.t] out (Bench.output params.bench) then (
          stats.solved <- true;
          raise Done);

        let prev_n_states = stats.n_states in
        State_set.add states out;
        stats.n_states <- State_set.length states;

        if stats.n_states > prev_n_states && stats.n_states mod 1000 = 0 then (
          let points = State_set.to_list states in
          let start = Time.now () in
          let vp = Vpt.(create Random points) in
          let end_ = Time.now () in
          assert (Vpt.check vp);
          print_s [%message (vp : Vpt.t)];
          print_s
            [%message
              (Time.diff end_ start : Time.Span.t) (stats.n_states : int)])
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
