open Std
open Cad_ext
module Synth = Local_search_diverse.Make (Cad_ext)

let (size : Scene.Size.t) = { xres = 30; yres = 30; xlen = 30.0; ylen = 30.0 }

let jaccard (v : Value.t) (v' : Value.t) =
  match (v, v') with Scene s, Scene s' -> Scene.jaccard s s' | _ -> Float.infinity

let synth ?(use_rules = true) ?(use_normalize = true) ?(use_distance = `True) cost (target : Value.t) (ops : Op.t list)
    =
  let rules = if use_rules then [] else [] in

  let normalize (p : Op.t Program.t) : Op.t Program.t =
    match p with Apply (Int x, []) -> Apply (Int (x / 5 * 5), []) | p -> p
  in
  let normalize = Option.some_if use_normalize normalize in

  let ectx = Value.Ctx.create size in

  let distance =
    match use_distance with `True -> jaccard | `Close -> fun _ _ -> 0.0 | `Far -> fun _ _ -> Float.infinity
  in
  let ctx =
    Synth.Ctx.create ~verbose:false ~distance ~max_cost:cost ~rules ?normalize ~search_thresh:(Top_k 3) ectx ops target
  in
  match (new Synth.synthesizer ctx)#run with
  | Some p -> eprint_s [%message (p : Op.t Program.t)]
  | None -> failwith "synthesis failed"
