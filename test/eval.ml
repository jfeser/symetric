open Staged_synth
open Base_quickcheck

module N_ary_tree : sig
  type 'a t = Node of 'a * 'a t list [@@deriving quickcheck]
end = struct
  type 'a t = Node of 'a * 'a t list

  let quickcheck_generator quickcheck_generator_key =
    (* In order to bound recursion depth, we rely on the fact that the [list] generator
       always generates elements at strictly smaller sizes than the list itself. *)
    Generator.fixed_point (fun quickcheck_generator ->
        [%quickcheck.generator: key * t list]
        |> Generator.map ~f:(fun (key, list) -> Node (key, list)))

  let quickcheck_observer quickcheck_observer_key =
    (* For polymorphic types, we cannot directly derive an observer from a hash function.
       So we use [Observer.fixed_point] instead. *)
    Observer.fixed_point (fun quickcheck_observer ->
        [%quickcheck.observer: key * t list]
        |> Observer.unmap ~f:(fun (Node (key, list)) -> (key, list)))

  let quickcheck_shrinker quickcheck_shrinker_key =
    (* We can define a simple shrinker using [Shrinker.fixed_point]. It won't include the
       strategy above of replacing a recursive node with its children. *)
    Shrinker.fixed_point (fun quickcheck_shrinker ->
        [%quickcheck.shrinker: key * t list]
        |> Shrinker.map
             ~f:(fun (key, list) -> Node (key, list))
             ~f_inverse:(fun (Node (key, list)) -> (key, list)))
end

open N_ary_tree

let conc params =
  let rec eval (Node (op, args)) =
    Conc.eval params op @@ List.map args ~f:eval
  in
  eval

let abs params =
  let rec eval (Node (op, args)) =
    Abs.eval params op @@ List.map args ~f:eval
  in
  eval

let%test_unit "abs approximates conc" = Test.run_exn
