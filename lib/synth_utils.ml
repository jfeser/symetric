let timed f =
  let start = Time.now () in
  let ret = f () in
  let end_ = Time.now () in
  (ret, Time.diff end_ start)

let time f =
  let _, t = timed f in
  t

exception Break

let break _ = raise Break

let () = Caml.Sys.(set_signal sigint (Signal_handle break))

let print_json = Yojson.Basic.to_channel Out_channel.stdout

let run_synth constr params print () =
  Random.set_state @@ Random.State.make [| Params.(get params seed) |];
  let synth = constr params in
  let output, time = timed (fun () -> try synth#run with Break -> None) in
  print output;
  Params.(get params runtime) := time;
  if Params.(get params print_json) then print_json @@ Dumb_params.json params
