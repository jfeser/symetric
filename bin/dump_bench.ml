open! Core
open Symetric

let pp_iters_fixed =
  let pp fmt n = Format.fprintf fmt "%10Ld" n in
  (pp, 10)

let iters f =
  let pp, width = pp_iters_fixed in
  f ~width pp

let no_total_counter ?message ?pp ?width ?(sampling_interval = 1) () =
  let open Progress in
  let open Line in
  let box =
    match width with Some width -> box_fixed width | None -> box_winsize ~fallback:80
  in
  list
    ((Option.map message ~f:const |> Option.to_list)
    @ (Option.map pp ~f:(fun f -> f of_pp) |> Option.to_list))
  |> box |> periodic sampling_interval |> accumulator Int64.( + ) 0L |> make ~init:0L

let bar msg = no_total_counter ~message:msg ~pp:iters ()
let tbar msg total = Progress.counter ~total ~message:msg ~pp:iters ()

let sequence_progress bar =
  Sequence.unfold_with ~init:() ~f:(fun () x ->
      bar 1L;
      Yield (x, ()))

module Make (Lang : Lang_intf.S_with_gen) = struct
  open Lang

  let eval_program params = Program.eval (Value.eval params)

  let has_noop params p =
    let exception Noop in
    let rec eval (Program.Apply (op, args)) =
      let args = List.map ~f:eval args in
      let v = Value.eval params op args in
      if List.mem args v ~equal:[%compare.equal: Value.t] then raise Noop else v
    in
    try
      (eval p : Value.t) |> ignore;
      false
    with Noop -> true

  let irreducible params p =
    let values = Hashtbl.create (module Value) in
    let rec eval (Program.Apply (op, args) as p) =
      let args = List.map ~f:eval args in
      let out = Value.eval params op args in
      Hashtbl.update values out ~f:(fun ps -> p :: Option.value ps ~default:[]);
      out
    in
    (eval p : Value.t) |> ignore;
    not @@ Hashtbl.existsi values ~f:(fun ~key:_ ~data:ps -> List.length ps > 1)

  (** 
@param size program size
@param nprim number of primitives available
@param n number of programs to generate 
*)
  let random ~size ~n ~dir params =
    Progress.(
      with_reporters
        (bar "samples" / bar "irreducible programs"
        / tbar "unique programs" (Int64.of_int n)))
    @@ fun ((samples, irreducible_progs), unique_progs) ->
    Sequence.unfold ~init:() ~f:(fun () -> Some (Gen.random_program params size, ()))
    |> Sequence.filter_map ~f:Fun.id |> sequence_progress samples
    |> Sequence.filter ~f:(fun (p, _) ->
           (not (has_noop params p)) && irreducible params p)
    |> sequence_progress irreducible_progs
    |> Sequence.map ~f:(fun (prog, ops) -> (prog, ops, eval_program params prog))
    |> Sequence.unfold_with
         ~init:(Set.empty (module Value))
         ~f:(fun seen ((_, _, out) as x) ->
           if Set.length seen >= n then Done
           else if Set.mem seen out then Skip seen
           else Yield (x, Set.add seen out))
    |> sequence_progress unique_progs
    |> Sequence.map ~f:(fun (prog, ops, out) -> Gen.to_bench params ops prog out)
    |> Sequence.iteri ~f:(fun i b -> Bench.save [%string "%{dir}/scene_%{i#Int}.sexp"] b)
end

let random_cli (module Lang : Lang_intf.S_with_gen) =
  let open Command in
  let open Let_syntax in
  basic ~summary:"Random benchmarks"
    [%map_open
      let params = Dumb_params.Spec.cli Lang.Gen.spec
      and seed = flag "seed" (optional_with_default 0 int) ~doc:" random seed"
      and size = flag "size" (required int) ~doc:" program size"
      and dir = flag "dir" (required string) ~doc:" output directory"
      and n = anon ("n" %: int) in
      fun () ->
        Random.init seed;
        Unix.mkdir_p dir;

        let open Make (Lang) in
        random params ~size ~n ~dir]

let () =
  Command.group ~summary:"Dump benchmarks"
    [ ("cad", random_cli (module Cad)); ("tensor", random_cli (module Tensor)) ]
  |> Command.run
