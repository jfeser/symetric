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

let synth ?(use_normalize = true) ?(use_distance = `True) size (target : Scene.t) (ops : Op.t list) =
  let ectx = Value.Ctx.create size in

  let eval = Program.eval (Value.eval ectx) in

  let unnormalize =
    let open Op in
    if use_normalize then
      let apply = function
        | Program.Apply (Int x, []) ->
            if x < -10 then [ int (x + 1) ]
            else if x > 30 then [ int (x - 1) ]
            else [ int (x - 1); int (x + 1); int (x + 5); int (x - 5); int (x + 10); int (x - 10) ]
        | Apply (Circle, [ Apply (Int x, []); Apply (Int y, []); Apply (Int r, []) ]) ->
            let lx = Int.round_down ~to_multiple_of:5 @@ (x - r)
            and ly = Int.round_down ~to_multiple_of:5 @@ (y - r)
            and hx = Int.round_up ~to_multiple_of:5 @@ (x + r)
            and hy = Int.round_up ~to_multiple_of:5 @@ (y + r) in
            [ rect lx ly hx hy ]
        | Apply (Rect, [ Apply (Int lx, []); Apply (Int ly, []); Apply (Int hx, []); Apply (Int hy, []) ]) ->
            let x = (lx + hx) / 2 and y = (ly + hy) / 2 in
            [ circle x y ((hx - lx) / 2); circle x y ((hy - ly) / 2) ]
        | _ -> []
      in
      Some apply
    else None
  in

  let normalize : Op.t Program.t -> Op.t Program.t =
   fun p ->
    let open Op in
    match eval p with
    | Int x -> Apply (Int (Int.round_nearest x ~to_multiple_of:10), [])
    | Scene s -> (
        if Bitarray.(hamming_weight @@ and_ (Scene.pixels s) (not (Scene.pixels target))) > 0 then empty
        else
          match p with
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
          | p -> p)
  in
  let normalize = Option.some_if use_normalize normalize in

  let target = Value.Scene target in

  let distance (v : Value.t) (v' : Value.t) =
    match (v, v') with
    | Int x, Int x' -> if abs (x - x') < 3 then 0.0 else Float.infinity
    | Scene _, Scene _ -> jaccard v v'
    | _ -> Float.infinity
  in

  let distance =
    match use_distance with `True -> distance | `Close -> fun _ _ -> 0.0 | `Far -> fun _ _ -> Float.infinity
  in
  let match_prog msg (prog : Op.t Program.t) state =
    match prog with
    | ( Apply (Union, [ Apply (Rect, _); Apply (Repl, Apply (Rect, _) :: _) ])
      | Apply (Union, [ Apply (Repl, Apply (Rect, _) :: _); Apply (Rect, _) ])
      | Apply (Union, [ Apply (Circle, _); Apply (Repl, Apply (Circle, _) :: _) ])
      | Apply (Union, [ Apply (Repl, Apply (Circle, _) :: _); Apply (Circle, _) ])
      | Apply (Union, [ _; Apply (Repl, _) ])
      | _ ) as prog ->
        Fmt.epr "@[<hv>%s %f:@ %a@.%a@]%!" msg (distance state target) (Program.pp Op.pp) prog (Value.pp ectx) state;
        print_s [%message (prog : Op.t Program.t)]
  in
  let on_close_state prog state =
    let d = distance state target in
    if Float.(d < infinity) then match_prog "Before local search" prog state
  in
  let after_local_search = match_prog "After local search" in

  let on_existing state state' =
    Fmt.epr "@[<hv>Close existing state:@ %a@.%a@]%!" (Value.pp ectx) state (Value.pp ectx) state'
  in
  let after_local_search = match_prog "After local search" in

  let on_groups groups =
    List.iter groups ~f:(function
      | (value, _, _) :: rest ->
          Fmt.epr "@[<hv>Group representative %f:@ %a@]\n%!" (distance target value) (Value.pp ectx) value;
          List.iter rest ~f:(fun (v, _, _) ->
              Fmt.epr "@[<hv>Group member %f:@ %a@]\n%!" (distance target v) (Value.pp ectx) v)
      | _ -> ())
  in

  let ctx =
    Synth.Ctx.create ~search_width:100 ~verbose:true ~distance ?unnormalize ?normalize
      ~search_thresh:(Top_k 15) (* ~on_groups *) ~after_local_search (* ~on_close_state *)
      (* (\* ~on_close_state ~after_local_search *\) *)
      (* (\* ~on_groups *\) ~on_existing *)
      ectx ops target
  in
  match (new Synth.synthesizer ctx)#run with
  | Some p -> eprint_s [%message (p : Op.t Program.t)]
  | None -> failwith "synthesis failed"
