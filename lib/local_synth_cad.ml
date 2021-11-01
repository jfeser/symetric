open! Std
open Cad_ext
module Synth = Local_search_diverse.Make (Cad_ext)

let jaccard (v : Value.t) (v' : Value.t) =
  match (v, v') with Scene s, Scene s' -> Scene.jaccard s s' | _ -> Float.infinity

let norm_int x = Int.round ~dir:`Down x ~to_multiple_of:10

let synth ?(use_rules = false) ?(use_normalize = false) ?(use_distance = `True) size (target : Value.t)
    (ops : Op.t list) =
  let rules =
    let open Local_search.Pattern in
    if use_rules then List.init 29 ~f:(fun i -> (Apply (Op.Int i, []), Apply (Op.Int (i + 1), []))) else []
  in

  let normalize (p : Op.t Program.t) : Op.t Program.t =
    match p with Apply (Int x, []) -> Apply (Int (norm_int x), []) | p -> p
  in
  let normalize = Option.some_if use_normalize normalize in

  let ectx = Value.Ctx.create size in

  let distance =
    match use_distance with `True -> jaccard | `Close -> fun _ _ -> 0.0 | `Far -> fun _ _ -> Float.infinity
  in
  (* let on_close_state prog state = *)
  (*   Fmt.epr "@[<hv>Found close state %f:@ %a@.%a@]%!" (jaccard state target) (Program.pp Op.pp) prog (Value.pp ectx) *)
  (*     state *)
  (* in *)
  (* let after_local_search prog state = *)
  (*   Fmt.epr "@[<hv>After local search:@ %a@.%a@]%!" (Program.pp Op.pp) prog (Value.pp ectx) state *)
  (* in *)
  let ctx =
    Synth.Ctx.create ~search_width:100 ~verbose:false ~distance ~rules ?normalize ~search_thresh:(Top_k 3) ectx ops
      target
  in
  match (new Synth.synthesizer ctx)#run with
  | Some p -> eprint_s [%message (p : Op.t Program.t)]
  | None -> failwith "synthesis failed"
