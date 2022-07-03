open Std

let timed action f =
  let start = Time.now () in
  Exn.protect ~f ~finally:(fun () ->
      let runtime = Time.diff (Time.now ()) start in
      match action with `Set r -> r := runtime | `Add r -> r := Time.Span.(!r + runtime))

exception Break

let break _ = raise Break
let () = Caml.Sys.(set_signal sigint (Signal_handle break))
let print_json = Yojson.Basic.to_channel Out_channel.stdout

let rec luby_cutoff base max_pow =
  if max_pow = 0 then Iter.singleton 1.0
  else
    Iter.Infix.(
      luby_cutoff base (max_pow - 1)
      <+> luby_cutoff base (max_pow - 1)
      <+> Iter.singleton (Float.int_pow base max_pow))

let%expect_test "" =
  print_s [%message (luby_cutoff 2.0 4 : float Iter.t)];
  [%expect
    {|
    ("luby_cutoff 2.0 4"
     (1 1 2 1 1 2 4 1 1 2 1 1 2 4 8 1 1 2 1 1 2 4 1 1 2 1 1 2 4 8 16)) |}]

let geometric_cutoff base =
  Iter.init (fun x -> Float.(to_int @@ (base ** of_int Int.(x + 1))))

let%expect_test "" =
  print_s [%message (Iter.take 10 @@ geometric_cutoff 1.3 : int Iter.t)];
  [%expect {| ("(Iter.take 10) @@ (geometric_cutoff 1.3)" (1 1 2 2 3 4 6 8 10 13)) |}]
