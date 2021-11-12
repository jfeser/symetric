open! Std
open Cad_ext
module Synth = Local_search_diverse.Make (Cad_ext)

let jaccard (v : Value.t) (v' : Value.t) =
  match (v, v') with Scene s, Scene s' -> Scene.jaccard s s' | _ -> Float.infinity

let jaccard_edges size (v : Value.t) (v' : Value.t) =
  match (v, v') with Scene s, Scene s' -> Scene.(jaccard (edges size s) (edges size s')) | _ -> Float.infinity

let corner size (v : Value.t) (v' : Value.t) =
  match (v, v') with
  | Scene s, Scene s' ->
      let c = Scene.corners size s and c' = Scene.corners size s' in
      let c = List.map ~f:Scene.count c and c' = List.map ~f:Scene.count c' in
      List.map2_exn c c' ~f:(fun v v' -> (v - v') * (v - v'))
      |> List.sum (module Int) ~f:Fun.id
      |> Float.of_int |> Float.sqrt
  | _ -> Float.infinity

let norm_int x = Int.round ~dir:`Down x ~to_multiple_of:3

let synth ?(use_rules = true) ?(use_normalize = true) ?(use_distance = `True) size (target : Value.t) (ops : Op.t list)
    =
  let rules =
    let open Local_search.Pattern in
    if use_rules then
      let int_rewrite = List.init 29 ~f:(fun i -> (Apply (Op.Int i, []), Apply (Op.Int (i + 1), []))) in
      int_rewrite
    else []
  in

  let normalize (p : Op.t Program.t) : Op.t Program.t =
    match p with
    | Apply (Circle, [ Apply (Int x, []); Apply (Int y, []); Apply (Int r, []) ]) ->
        Apply
          ( Circle,
            [
              Apply (Int (Int.round_down x ~to_multiple_of:5), []);
              Apply (Int (Int.round_down y ~to_multiple_of:5), []);
              Apply (Int (Int.round_down r ~to_multiple_of:5), []);
            ] )
    | Apply (Repl, [ p; dx; dy; _ ]) -> Apply (Repl, [ p; dx; dy; Apply (Int 5, []) ])
    | p -> p
  in
  let normalize = Option.some_if use_normalize normalize in

  let ectx = Value.Ctx.create size in

  let distance =
    match use_distance with
    | `True -> fun v v' -> (* jaccard_edges size v v' +. *) jaccard v v' +. corner size v v'
    | `Close -> fun _ _ -> 0.0
    | `Far -> fun _ _ -> Float.infinity
  in
  (* let on_close_state prog state = *)
  (*   Fmt.epr "@[<hv>Found close state %f:@ %a@.%a@]%!" (distance state target) (Program.pp Op.pp) prog (Value.pp ectx) *)
  (*     state *)
  (* in *)
  let after_local_search prog state =
    Fmt.epr "@[<hv>After local search %f:@ %a@.%a@]%!" (distance state target) (Program.pp Op.pp) prog (Value.pp ectx)
      state
  in
  let on_groups groups =
    List.iter groups ~f:(function
      | (value, _, _) :: rest ->
          Fmt.epr "@[<hv>Group representative %f:@ %a@]\n%!" (corner size target value) (Value.pp ectx) value;
          List.iter rest ~f:(fun (v, _, _) ->
              Fmt.epr "@[<hv>Group member %f:@ %a@]\n%!" (corner size target value) (Value.pp ectx) v)
      | _ -> ())
  in
  let ctx =
    Synth.Ctx.create ~search_width:1000 ~verbose:true ~distance ~rules ?normalize ~search_thresh:(Top_k 15) ~on_groups
      ~after_local_search ectx ops target
  in
  match (new Synth.synthesizer ctx)#run with
  | Some p -> eprint_s [%message (p : Op.t Program.t)]
  | None -> failwith "synthesis failed"
