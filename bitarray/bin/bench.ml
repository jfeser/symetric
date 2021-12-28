open! Core
open Core_bench
open Bitarray

let () =
  Random.self_init ();
  let random_native () = Native.init ~f:(fun _ -> Random.bool ()) (64 * 4) in
  let random_vect () = Vectorized.init ~f:(fun _ -> Random.bool ()) (64 * 4) in

  let n1 = random_native () and n2 = random_native () in
  let v1 = random_vect () and v2 = random_vect () in
  let and_native = Bench.Test.create ~name:"and-native" (fun () -> Native.and_ n1 n2) in
  let and_vect = Bench.Test.create ~name:"and-vect" (fun () -> Vectorized.and_ v1 v2) in
  let hamming_native = Bench.Test.create ~name:"hamming-native" (fun () -> Native.hamming_weight n1) in
  let hamming_vect = Bench.Test.create ~name:"hamming-vect" (fun () -> Vectorized.hamming_weight v1) in

  Command.run
    (Bench.make_command
       [
         Bench.Test.create_group ~name:"and" [ and_native; and_vect ];
         Bench.Test.create_group ~name:"hamming" [ hamming_native; hamming_vect ];
       ])
