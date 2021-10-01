module Synth = Baseline.Make (Tensor)

let synth cost (target : Tensor.Value.t) (ops : Tensor.Op.t list) =
  let open Tensor in
  let ectx = Value.Ctx.create () in
  let ctx = Synth.Ctx.create ~verbose:true ~max_cost:cost ectx ops (`Value target) in
  match (new Synth.synthesizer ctx)#run with
  | Some p -> print_s [%message (p : Op.t Program.t)]
  | None -> failwith "synthesis failed"
