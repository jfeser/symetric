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
    let sample_counter () =
      let prog =
        List.random_element ~random_state:(params ss).random_state target
        |> Option.value_exn |> sample ss
      in
      let out = Cad_conc.eval_program (params ss) prog in
      if [%compare.equal: Conc.t] out (Bench.output (params ss).bench) then
        raise (Found_solution prog)
      else out
    in
    List.init n ~f:(fun _ -> sample_counter ())

  let most_common exs =
    List.fold_left exs
      ~init:(Map.empty (module Vector2))
      ~f:(fun m ->
        Set.fold ~init:m
          ~f:(Map.update ~f:(function Some ctr -> ctr + 1 | None -> 1)))
    |> Map.to_alist
    |> List.max_elt ~compare:(fun (_, c) (_, c') -> [%compare: int] c c')
    |> Option.value_exn |> Tuple.T2.get1

  let refine ss target =
    let counter = sample_counters ss target 1000 |> most_common in
    print_s [%message (counter : Vector2.t)];
    raise_s [%message (List.map target ~f:(State.to_message ss) : Sexp.t list)]
end
