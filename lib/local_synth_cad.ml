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

let synth ?(use_normalize = true) ?(use_distance = `True) size (target : Value.t) (ops : Op.t list) =
  let int x : Op.t Program.t = Apply (Int x, []) in

  let unnormalize =
    if use_normalize then
      let apply = function
        | Program.Apply (Op.Int x, []) ->
            if x < -10 then [ Program.Apply (Op.Int (x + 1), []) ]
            else if x > 30 then [ Apply (Int (x - 1), []) ]
            else [ Program.Apply (Op.Int (x - 1), []); Apply (Int (x + 1), []) ]
        | Apply (Circle, [ Apply (Int x, []); Apply (Int y, []); Apply (Int r, []) ]) ->
            let lx = Int.round_down ~to_multiple_of:5 @@ (x - r)
            and ly = Int.round_down ~to_multiple_of:5 @@ (y - r)
            and hx = Int.round_up ~to_multiple_of:5 @@ (x + r)
            and hy = Int.round_up ~to_multiple_of:5 @@ (y + r) in
            [ Apply (Rect, [ int lx; int ly; int hx; int hy ]) ]
        | Apply (Op.Union, [ s; s' ]) -> [ s; s' ]
        | _ -> []
      in
      Some apply
    else None
  in

  let normalize : Op.t Program.t -> Op.t Program.t = function
    | Apply (Circle, [ Apply (Int x, []); Apply (Int y, []); Apply (Int r, []) ]) as c ->
        let lx = Int.round_down ~to_multiple_of:5 @@ (x - r)
        and ly = Int.round_down ~to_multiple_of:5 @@ (y - r)
        and hx = Int.round_up ~to_multiple_of:5 @@ (x + r)
        and hy = Int.round_up ~to_multiple_of:5 @@ (y + r) in
        Apply (Union, [ c; Apply (Rect, [ int lx; int ly; int hx; int hy ]) ])
    | Apply (Rect, [ Apply (Int lx, []); Apply (Int ly, []); Apply (Int hx, []); Apply (Int hy, []) ]) ->
        Apply
          ( Rect,
            [
              Apply (Int (Int.round_down lx ~to_multiple_of:5), []);
              Apply (Int (Int.round_down ly ~to_multiple_of:5), []);
              Apply (Int (Int.round_up hx ~to_multiple_of:5), []);
              Apply (Int (Int.round_up hy ~to_multiple_of:5), []);
            ] )
    | Apply (Repl, [ p; dx; dy; _ ]) -> Apply (Repl, [ p; dx; dy; Apply (Int 5, []) ])
    | Apply (Int x, []) -> Apply (Int (Int.round_nearest x ~to_multiple_of:10), [])
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
  let on_close_state prog state =
    Fmt.epr "@[<hv>Found close state %f:@ %a@.%a@]%!" (distance state target) (Program.pp Op.pp) prog (Value.pp ectx)
      state
  in
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
    Synth.Ctx.create ~search_width:500 ~verbose:false ~distance ?unnormalize ?normalize ~search_thresh:(Top_k 15)
      ~after_local_search ectx ops target
  in
  match (new Synth.synthesizer ctx)#run with
  | Some p -> eprint_s [%message (p : Op.t Program.t)]
  | None -> failwith "synthesis failed"
