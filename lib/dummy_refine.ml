open Params

module Make
    (Search_state : Search_state_intf.S
                      with type op = Cad_concrete.Op.t
                       and type params = Cad_concrete.params) =
struct
  open Search_state
  open Cad_concrete

  exception Found_solution of Op.t Program.t [@@deriving sexp]

  module Refinement = struct
    type s = { old : Set.M(Abs).t; new_ : Set.M(Abs).t }
    [@@deriving compare, sexp]

    type t = s Map.M(Search_state.Args).t [@@deriving sexp_of]
  end

  let sample ss target =
    let expected = Bench.output (params ss).bench in
    let sample_counter () =
      let prog =
        List.random_element ~random_state:(params ss).random_state target
        |> Option.value_exn |> sample ss
      in
      let out = Cad_conc.eval_program (params ss) prog in
      if [%compare.equal: Conc.t] out expected then raise (Found_solution prog)
    in
    while true do
      sample_counter ()
    done

  let refine ss target =
    try
      sample ss target;
      failwith ""
    with Found_solution p -> Second p
end
