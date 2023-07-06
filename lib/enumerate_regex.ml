open Std

module Stats = struct
  type t = { runtime : Timer.t } [@@deriving yojson_of]

  let create () = { runtime = Timer.create () }
end

let stats = Stats.create ()

let write_output out m_prog =
  let program_size =
    Option.map m_prog ~f:(fun p -> Float.of_int @@ Program.size p)
    |> Option.value ~default:Float.nan
  in
  let program_json =
    Option.map m_prog ~f:(fun p -> `String (Sexp.to_string @@ Regex.serialize p))
    |> Option.value ~default:`Null
  in
  let open Yojson in
  let json =
    `Assoc
      [
        ("method", `String "enumeration");
        ("program", program_json);
        ("program_size", `Float program_size);
        ("stats", [%yojson_of: Stats.t] stats);
      ]
  in
  Out_channel.with_file out ~f:(fun ch -> Safe.to_channel ch json)

let synthesize (synth_params : Baseline.Params.t) bench =
  let vctx, ops = bench in
  let module Dsl = struct
    include Regex

    module Value = struct
      include Value

      let eval = eval vctx
    end

    let operators = Op.default_operators 15 @ ops
  end in
  Baseline.synthesize
    (module Dsl)
    synth_params
    (`Pred (fun _ v -> Float.(Regex.Value.target_distance vctx v = 0.)))

(* let cmd = *)
(*   let open Command.Let_syntax in *)
(*   Command.basic ~summary:"Solve regex problems with enumeration." *)
(*     [%map_open *)
(*       let sketch = flag "-sketch" (required string) ~doc:" regex sketch" *)
(*       and out = flag "-out" (required string) ~doc:" output file" in *)
(*       fun () -> *)
(*         let ctx, ops = Regex_bench.load_sketch_bench sketch In_channel.stdin in *)
(*         let synth_ctx = *)
(*           Synth.Ctx.create ctx *)
(*             (Op.default_operators 15 @ ops) *)
(*             (`Pred (fun _ v -> Float.(Regex.Value.target_distance ctx v = 0.))) *)
(*         in *)
(*         write_output out None; *)
(*         Timer.start stats.runtime; *)
(*         let synth = new Synth.synthesizer synth_ctx in *)
(*         let p = synth#run in *)
(*         Timer.stop stats.runtime; *)
(*         write_output out p] *)
