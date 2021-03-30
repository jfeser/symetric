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
    type elem =
      | Remove_edge of G.V.t * G.V.t
      | Add_edge of G.E.t
      | Add_merge of State.t list * Abs.t
    [@@deriving compare, sexp_of]

    type t = elem list [@@deriving compare, sexp_of]
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

  let reduce_while l ~f =
    let rec loop g gs = function
      | [] -> g :: gs
      | x :: xs -> (
          match f g x with
          | Some g' -> loop g' gs xs
          | None -> loop x (g :: gs) xs )
    in
    match l with [] -> [] | x :: xs -> loop x [] xs

  let rec update l ~f =
    match l with
    | [] -> None
    | x :: xs -> (
        match f x with
        | Some x' -> Some (x' :: xs)
        | None -> (
            match update ~f xs with Some xs' -> Some (x :: xs') | None -> None )
        )

  let summarize ss states =
    List.permute states
    |> List.fold ~init:[] ~f:(fun groups s ->
           let sabs = State.state ss s in
           let groups' =
             update groups ~f:(fun (abs, sts) ->
                 let abs' = Abs.lub abs sabs in
                 if
                   not @@ Abs.contains abs' (Cad_bench.output (params ss).bench)
                 then Some (abs', s :: sts)
                 else None)
           in
           match groups' with
           | Some groups -> groups
           | None -> (sabs, [ s ]) :: groups)

  let rec refine_merge ss counter output args_v =
    let inputs =
      G.succ (graph ss) (Node.of_args args_v)
      |> List.map ~f:Node.to_state_exn
      |> List.permute
    in
    let t, f, m =
      List.partition3_map inputs ~f:(fun state_v ->
          let abs = State.state ss state_v in
          match Cad_abs.implies abs counter with
          | True -> `Fst state_v
          | False -> `Snd state_v
          | Maybe -> `Trd state_v)
    in
    let refn =
      G.succ_e (graph ss) (Node.of_args args_v)
      |> List.map ~f:(fun (v, _, v') -> Remove_edge (v, v'))
    in
    let refn =
      Remove_edge (Node.of_state output, Node.of_args args_v) :: refn
    in
    let t_groups =
      List.map t ~f:(fun v -> (State.state ss v, [ v ]))
      |> reduce_while ~f:(fun (a, s) (a', s') ->
             let a'' = Abs.lub a a' in
             match Cad_abs.implies a'' counter with
             | True -> Some (a'', s @ s')
             | _ -> None)
    in
    let f_groups =
      List.map f ~f:(fun v -> (State.state ss v, [ v ]))
      |> reduce_while ~f:(fun (a, s) (a', s') ->
             let a'' = Abs.lub a a' in
             match Cad_abs.implies a'' counter with
             | False -> Some (a'', s @ s')
             | _ -> None)
    in
    let merge_refn groups =
      List.map groups ~f:(fun (state, inputs) -> Add_merge (inputs, state))
    in
    merge_refn t_groups @ merge_refn f_groups @ refn
    @ List.concat_map m ~f:(refine_state ss counter)

  and refine_args ss p output args_v =
    let inputs =
      G.succ (graph ss) (Node.of_args args_v) |> List.map ~f:Node.to_state_exn
    in
    match Args.label ss args_v with
    | Merge -> refine_merge ss p output args_v
    | Op (Union | Inter) -> List.concat_map inputs ~f:(refine_state ss p)
    | Op ((Circle _ | Rect _) as op) ->
        let conc = Cad_conc.eval (params ss) op [] in
        let old = State.state ss output in
        let new_ =
          let module Boxes = Cad_abs.Boxes in
          let abs =
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
          State.create_or_get ss abs (State.cost ss output)
            (State.type_ ss output)
          |> Is_fresh.unwrap
        in

        [
          Remove_edge (Node.of_state output, Node.of_args args_v);
          Add_edge (Node.of_state new_, -1, Node.of_args args_v);
        ]

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
      eprint_s
        [%message
          (List.map target ~f:(State.to_message ss) : Sexp.t list)
            (counter : K.t)];
      let counter, _ = counter in
      List.concat_map target ~f:(refine_state ss counter)
      |> List.dedup_and_sort ~compare:[%compare: elem]
      |> Either.first
    with Found_solution p -> Second p

  let summarize = Some summarize
end
