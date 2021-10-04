open Std

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

let rec luby_cutoff base max_pow =
  if max_pow = 0 then Iter.singleton 1.0
  else
    Iter.Infix.(
      luby_cutoff base (max_pow - 1) <+> luby_cutoff base (max_pow - 1) <+> Iter.singleton (Float.int_pow base max_pow))

let%expect_test "" =
  print_s [%message (luby_cutoff 2.0 4 : float Iter.t)];
  [%expect {|
    ("luby_cutoff 2 4"
     (1 1 2 1 1 2 4 1 1 2 1 1 2 4 8 1 1 2 1 1 2 4 1 1 2 1 1 2 4 8 16)) |}]

let geometric_cutoff base = Iter.init (fun x -> Float.(to_int @@ (base ** of_int Int.(x + 1))))

let%expect_test "" =
  print_s [%message (Iter.take 10 @@ geometric_cutoff 1.3 : int Iter.t)];
  [%expect {| ("(Iter.take 10) @@ (geometric_cutoff 1.3)" (1 1 2 2 3 4 6 8 10 13)) |}]
