let synthesize (synth_params : Baseline.Params.t) target =
  let module Dsl = struct
    include Tower

    module Value = struct
      include Value

      let eval = eval (Value.Ctx.create ~target ())
    end
  end in
  Baseline.synthesize (module Dsl) synth_params (`Value target)

(* let cmd = *)
(*   let open Command.Let_syntax in *)
(*   Command.basic ~summary:"Solve tower problems with enumeration." *)
(*     [%map_open *)
(*       let out = flag "-out" (required string) ~doc:" output file" in *)
(*       fun () -> *)
(*         let ctx = Value.Ctx.create () in *)
(*         let target = *)
(*           Sexp.input_sexp In_channel.stdin *)
(*           |> Tower.parse *)
(*           |> Program.eval (Value.eval (Value.Ctx.create ())) *)
(*         in *)
(*         let operators = *)
(*           [ Op.Loop; Drop_v; Drop_h; Embed; Seq ] *)
(*           @ (Iter.int_range ~start:1 ~stop:8 *)
(*             |> Iter.map (fun i -> Op.Int i) *)
(*             |> Iter.to_list) *)
(*           @ (Iter.append *)
(*                (Iter.int_range ~start:(-8) ~stop:(-1)) *)
(*                (Iter.int_range ~start:1 ~stop:8) *)
(*             |> Iter.map (fun i -> Iter.of_list [ Op.Move_p i; Op.Move_s i ]) *)
(*             |> Iter.concat |> Iter.to_list) *)
(*         in *)
(*         let synth_ctx = Synth.Ctx.create ctx ~verbose:true operators (`Value target) in *)
(*         write_output out None; *)
(*         Timer.start stats.runtime; *)
(*         let synth = new Synth.synthesizer synth_ctx in *)
(*         let p = synth#run in *)
(*         Timer.stop stats.runtime; *)
(*         write_output out p] *)
