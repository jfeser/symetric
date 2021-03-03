open Ternary

module Make
    (Search_state : Search_state_intf.S
                      with type op = Cad.Op.t
                       and type abs = Cad.Abs.t
                       and type bench = Cad.Bench.t) =
struct
  open Cad
  open Search_state

  module Refinement = struct
    type s = { old : Set.M(Abs).t; new_ : Set.M(Abs).t }
    [@@deriving compare, sexp]

    type t = s Map.M(Search_state.Args).t [@@deriving sexp_of]
  end

  open Refinement

  exception Found_solution of Cad.Op.t Program.t

  let sample_counters ss target n =
    let expected = Bench.output (params ss).bench in
    let sample_counter () =
      let prog =
        List.random_element ~random_state:(params ss).random_state target
        |> Option.value_exn |> sample ss
      in
      let out = Cad_conc.eval_program (params ss) prog in
      if [%compare.equal: Conc.t] out expected then raise (Found_solution prog)
      else
        Map.merge out expected ~f:(fun ~key -> function
          | `Left _ | `Right _ -> None
          | `Both (x, x') -> if Bool.(x <> x') then Some x' else None)
        |> Map.to_alist |> List.map ~f:Tuple.T2.get1
    in
    List.init n ~f:(fun _ -> sample_counter ())

  let most_common exs =
    List.fold_left exs
      ~init:(Map.empty (module Vector2))
      ~f:(fun m ->
        List.fold ~init:m
          ~f:(Map.update ~f:(function Some ctr -> ctr + 1 | None -> 1)))
    |> Map.to_alist
    |> List.max_elt ~compare:(fun (_, c) (_, c') -> [%compare: int] c c')
    |> Option.value_exn |> Tuple.T2.get1

  let split p a =
    List.concat_map a ~f:(fun b ->
        if Box.contains b p then
          let open Float in
          let l = b.xmax - b.xmin and h = b.ymax - b.ymin in
          [
            {
              b with
              xmax = b.xmin + (l / 2.0) |> round_up;
              ymax = b.ymin + (h / 2.0) |> round_up;
            };
            {
              b with
              xmin = b.xmin + (l / 2.0) |> round_down;
              ymax = b.ymin + (h / 2.0) |> round_up;
            };
            {
              b with
              xmax = b.xmin + (l / 2.0) |> round_up;
              ymin = b.ymin + (h / 2.0) |> round_down;
            };
            {
              b with
              xmin = b.xmin + (l / 2.0) |> round_down;
              ymin = b.ymin + (h / 2.0) |> round_down;
            };
          ]
        else [ b ])

  let rec refine_args ss counter output args_v =
    let inputs =
      G.succ (graph ss) (Node.of_args args_v) |> List.map ~f:Node.to_state_exn
    in
    match Args.op ss args_v with
    | Union | Inter -> (
        match inputs with
        | [ v; v' ] ->
            if
              Abs.contains (State.state ss v)
                (Map.singleton (module Vector2) counter true)
            then refine_state ss counter v
            else refine_state ss counter v' )
    | Circle c ->
        [
          ( args_v,
            {
              old = Set.singleton (module Abs) @@ State.state ss output;
              new_ =
                Set.singleton (module Abs)
                @@ split counter @@ State.state ss output;
            } );
        ]
    | Rect r -> raise_s [%message "rect"]

  and refine_state ss counter state_v =
    let abs = State.state ss state_v in
    match Cad_abs.implies abs counter with
    | True | False -> []
    | Maybe ->
        G.succ (graph ss) (Node.of_state state_v)
        |> List.map ~f:Node.to_args_exn
        |> List.concat_map ~f:(refine_args ss counter state_v)

  let refine ss target =
    try
      let counter = sample_counters ss target 1000 |> most_common in
      List.concat_map target ~f:(refine_state ss counter)
      |> Map.of_alist_reduce
           (module Args)
           ~f:(fun x x' ->
             if [%compare.equal: Refinement.s] x x' then x else failwith "")
      |> Either.first
    with Found_solution p -> Second p
end
