open Shape
open Local_search
module Synth = Local_search_diverse.Make (Shape)

let synth (target : Shape.Value.t) ops n_pos =
  let ectx = Shape.Value.Ctx.{ n_pos } in
  let rules =
    let module P = Local_search.Pattern in
    List.concat_map Shape.colors ~f:(fun c ->
        List.filter_map Shape.colors ~f:(fun c' ->
            if Poly.(c <> c') then Some (P.Apply (Op.Color c, []), P.Apply (Op.Color c', [])) else None))
    @ List.concat_map ops ~f:(function
        | Op.Sides s as o ->
            List.filter_map ops ~f:(function
              | Op.Sides s' as o' -> if s <> s' then Some (P.Apply (o, []), P.Apply (o', [])) else None
              | _ -> None)
        | _ -> [])
  in
  print_s [%message (rules : Op.t Rule.t list)];
  let ctx =
    Synth.Ctx.create ~verbose:true ~distance:(Shape.Value.dist ectx) ~max_cost:25 ~rules ~search_thresh:(Top_k 20) ectx
      ops target
  in
  match (new Synth.synthesizer ctx)#run with
  | Some p -> print_s [%message (p : Shape.Op.t Program.t)]
  | None -> failwith "synthesis failed"
