open Std

let generate_benchmarks (type op ctx type_) ?(max_states = 100_000)
    (module Lang : Baseline.Lang_intf with type Value.Ctx.t = ctx and type Op.t = op and type Type.t = type_) ops ectx
    cost type_ =
  let module Synth = Baseline.Make (Lang) in
  let open Synth in
  let config = Ctx.create ~max_cost:cost ~verbose:true ectx ops (`Pred (fun _ _ -> false)) in
  let synth =
    object
      inherit synthesizer config as super

      method! insert_states cost states = super#insert_states cost @@ List.take (List.permute states) max_states
    end
  in
  ignore (synth#run : Lang.Op.t Program.t option);
  let search_state : Search_state.t = synth#get_search_state in
  let attr = Search_state.Attr.create cost type_ in
  Hashtbl.find search_state.values attr |> Option.map ~f:Iter.of_queue |> Option.value ~default:Iter.empty
  |> Iter.map (Search_state.program_exn search_state type_)
