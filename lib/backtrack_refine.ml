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
    type s = { old : Set.M(Abs).t; new_ : Set.M(Abs).t } [@@deriving sexp]

    type t = s Map.M(Search_state.Args).t [@@deriving sexp_of]
  end

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

  (* let refine_union ss counter output inputs = ()
   * 
   * let refine_inter ss counter output inputs = ()
   * 
   * let refine_circle ss counter output inputs c = ()
   * 
   * let refine_args ss counter output args_v =
   *   print_s [%message "refine args" (Args.to_message ss args_v : Sexp.t)];
   *   let inputs =
   *     G.succ (graph ss) (Node.of_args args_v)
   *     |> List.map ~f:Node.to_state_exn
   *     |> List.map ~f:(State.state ss)
   *   in
   *   match Args.op ss args_v with
   *   | Union -> refine_union ss counter output inputs
   *   | Inter -> refine_inter ss counter output inputs
   *   | Circle c -> refine_circle ss counter output inputs c
   * 
   * let refine_state ss counter state_v =
   *   let abs = State.state ss state_v in
   *   match Cad_abs.implies abs counter with
   *   | True | False -> ()
   *   | Maybe ->
   *       G.succ (graph ss) (Node.of_state state_v)
   *       |> List.map ~f:Node.to_args_exn
   *       |> List.iter ~f:(refine_args ss counter abs) *)

  let refine ss target =
    let counter = sample_counters ss target 1000 |> most_common in
    (* List.iter target ~f:(refine_state ss counter); *)
    print_s [%message (counter : Vector2.t)];
    raise_s [%message (List.map target ~f:(State.to_message ss) : Sexp.t list)]
end
