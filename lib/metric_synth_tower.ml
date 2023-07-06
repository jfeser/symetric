open Tower

let rewrite : Op.t P.t -> Op.t P.t list = function
  | Apply (Int x, []) ->
      if x < 1 then [ Apply (Int (x + 1), []) ]
      else if x > 8 then [ Apply (Int (x - 1), []) ]
      else [ Apply (Int (x + 1), []); Apply (Int (x - 1), []) ]
  | Apply (Move_s x, args) ->
      let ret = [] in
      let ret = if x > -8 then P.Apply (Op.Move_s (x - 1), args) :: ret else ret in
      let ret = if x < 8 then P.Apply (Op.Move_s (x + 1), args) :: ret else ret in
      ret
  | Apply (Move_p x, args) ->
      let ret = [] in
      let ret = if x > -8 then P.Apply (Op.Move_p (x - 1), args) :: ret else ret in
      let ret = if x < 8 then P.Apply (Op.Move_p (x + 1), args) :: ret else ret in
      ret
  | _ -> []

let synthesize (metric_params : Metric_synth.Params.t) target =
  let ctx = Value.Ctx.create ~target () in
  let module Dsl = struct
    include Tower

    module Value = struct
      include Value

      let eval = eval ctx
      let distance = distance ctx
      let target_distance = target_distance ctx
      let pp = Fmt.nop
    end

    let rewrite = rewrite
  end in
  Metric_synth.synthesize metric_params (module Dsl)

(* let cmd = *)
(*   let open Command.Let_syntax in *)
(*   Command.basic ~summary:"Solve CAD problems with metric synthesis." *)
(*     [%map_open *)
(*       let mk_params = Params.cmd in *)
(*       fun () -> *)
(*         Set_once.set_exn params [%here] @@ mk_params (); *)
(*         synthesize ()] *)
