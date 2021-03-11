open Ternary

module Make
    (Search_state : Search_state_intf.S
                      with type op = Cad.Op.t
                       and type abs = Cad.Abs.t
                       and type params = Cad.params) =
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
        Map.merge out expected ~f:(fun ~key:_ -> function
          | `Left _ | `Right _ -> None
          | `Both (x, x') -> if Bool.(x <> x') then Some x else None)
        |> Map.to_alist
    in
    List.init n ~f:(fun _ -> sample_counter ())

  let most_common m exs =
    List.fold_left exs ~init:(Map.empty m) ~f:(fun m ->
        List.fold ~init:m
          ~f:(Map.update ~f:(function Some ctr -> ctr + 1 | None -> 1)))
    |> Map.to_alist
    |> List.max_elt ~compare:(fun (_, c) (_, c') -> [%compare: int] c c')
    |> Option.value_exn |> Tuple.T2.get1

  let split p a =
    List.concat_map a ~f:(fun b ->
        if Box.contains b p then Box.split b else [ b ])

  let rec refine_args ss p output args_v =
    let inputs =
      G.succ (graph ss) (Node.of_args args_v) |> List.map ~f:Node.to_state_exn
    in
    match Args.op ss args_v with
    | Union | Inter -> (
        match inputs with
        | [ v; v' ] -> refine_state ss p v @ refine_state ss p v'
        | _ -> failwith "unexpected inputs" )
    | (Circle _ | Rect _) as op ->
        let conc = Cad_conc.eval (params ss) op [] in
        let old = State.state ss output in
        let new_ =
          let module Boxes = Cad_abs.Boxes in
          if Map.find_exn conc p then
            let lower =
              Boxes.of_list
              @@ Box.create_closed ~xmin:p.x ~xmax:p.x ~ymin:p.y ~ymax:p.y
                 :: Boxes.to_list old.lower
            in
            { old with lower }
          else
            let upper =
              Cad_abs.Boxes.to_list old.upper
              |> split p
              |> List.filter ~f:(fun b ->
                     Map.existsi conc ~f:(fun ~key ~data ->
                         data && Box.contains b key))
              |> Cad_abs.Boxes.of_list
            in
            { old with upper }
        in
        let new_ = Set.singleton (module Abs) new_
        and old = Set.singleton (module Abs) old in
        [ (args_v, { old; new_ }) ]

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
      let module K = struct
        module T = struct
          type t = Vector2.t * bool [@@deriving compare, hash, sexp]
        end

        include T
        include Comparator.Make (T)
      end in
      let counter = sample_counters ss target 1000 |> most_common (module K) in
      print_s
        [%message
          (List.map target ~f:(State.to_message ss) : Sexp.t list)
            (counter : K.t)];
      let counter, _ = counter in
      List.concat_map target ~f:(refine_state ss counter)
      |> Map.of_alist_reduce
           (module Args)
           ~f:(fun x x' ->
             if [%compare.equal: Refinement.s] x x' then x else failwith "")
      |> Either.first
    with Found_solution p -> Second p
end
