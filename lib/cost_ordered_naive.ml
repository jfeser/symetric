let ordered_subsets ~compare:c ~equal:e =
  let open Sequence in
  let rec ordered_subsets n s =
    match (n, s) with
    | 0, _ -> return []
    | _, [] -> empty
    | _, x :: xs ->
        let t = ordered_subsets n xs in
        let u = ordered_subsets (n - 1) s |> map ~f:(fun s -> x :: s) in
        merge_with_duplicates ~compare:c t u
        |> concat_map ~f:(function
             | Left x | Right x -> return x
             | Both (x, x') -> of_list [ x; x' ])
        |> remove_consecutive_duplicates ~equal:e
  in
  ordered_subsets

let sum = List.sum (module Int) ~f:Fun.id

let%expect_test "" =
  let s =
    ordered_subsets
      ~compare:(fun l l' -> compare (sum l) (sum l'))
      ~equal:[%compare.equal: int list] 3 [ 0; 2; 4; 4; 7 ]
  in
  print_s [%message (s : int list Sequence.t)];
  [%expect
    {|
    (s
     ((0 0 0) (0 0 2) (0 2 2) (0 0 4) (2 2 2) (0 2 4) (0 0 7) (2 2 4) (0 4 4)
      (0 2 7) (2 4 4) (2 2 7) (0 4 7) (4 4 4) (2 4 7) (0 7 7) (4 4 7) (2 7 7)
      (4 7 7) (7 7 7))) |}]

module Make
    (Lang : Lang_intf.S
              with type Conc.t = Cad.Conc.t
               and type bench = Cad.Bench.t) =
struct
  open Lang

  module State_set = struct
    type t = (Lang.Conc.t * int) list ref

    let create () = ref []

    let length x = List.length !x

    let add s ((x, c) as k) =
      let rec insert = function
        | [] -> [ k ]
        | ((x', c') as k') :: xs ->
            if
              compare c c' < 0
              || (compare c c' = 0 && [%compare: Lang.Conc.t] x x' < 0)
            then k :: k' :: xs
            else if [%compare: Lang.Conc.t] x x' = 0 then k' :: xs
            else k' :: insert xs
      in
      let l = length s in
      s := insert !s;
      let l' = length s in
      l' > l

    let to_list x = !x
  end

  let hamming_cost = Cad.Conc.hamming

  let size_cost costs = 1 + List.sum (module Int) costs ~f:Fun.id

  let cost ?(weight = 1.0) params costs state =
    (Float.of_int (hamming_cost params state)
     /. (Float.of_int @@ (params.bench.input.xmax * params.bench.input.ymax))
     *. weight
    +. Float.of_int (size_cost costs)
       /. Float.of_int params.max_cost
       *. (1.0 -. weight))
    *. 100.0
    |> Float.to_int

  let eval = Lang.Conc.eval

  exception Restart

  exception Done

  let fill_op (params : _ Params.t) states op =
    let min_found = ref Int.max_value in
    State_set.to_list states
    |> ordered_subsets
         ~compare:(fun l l' ->
           let sum = List.sum (module Int) ~f:(fun (_, x) -> x) in
           compare (sum l) (sum l'))
         ~equal:(fun l l' ->
           let ss, _ = List.unzip l and ss', _ = List.unzip l' in
           [%compare.equal: Lang.Conc.t list] ss ss')
         (Op.arity op)
    |> Sequence.iter ~f:(fun args ->
           let args, costs = List.unzip args in
           let out = eval params op args in
           if [%compare.equal: Lang.Conc.t] out (Bench.output params.bench) then (
             print_s [%message "solution"];
             Cad.Conc.pprint params Fmt.stdout out;
             raise Done);
           let c = cost params costs out in
           min_found := Int.min !min_found c;
           if
             State_set.add states (out, c)
             && List.for_all costs ~f:(fun c' -> !min_found < c')
           then raise Restart)

  let synth (params : _ Params.t) =
    let ops = Bench.ops params.bench and states = State_set.create () in

    let non_nil_ops =
      List.filter ops ~f:(fun op -> Op.arity op > 0)
      |> List.sort ~compare:(fun o o' -> compare (Op.arity o) (Op.arity o'))
    in
    let fill () =
      while true do
        print_s [%message "filling" (State_set.length states : int)];

        print_s [%message "top 10"];
        List.take (State_set.to_list states) 10
        |> List.iter ~f:(fun (s, _) -> Cad.Conc.pprint params Fmt.stdout s);
        print_s [%message "done"];

        try List.iter non_nil_ops ~f:(fill_op params states)
        with Restart -> ()
      done
    in

    try
      List.iter ops ~f:(fun op ->
          if Op.arity op = 0 then
            let state = eval params op [] in
            let c = cost params [] state in
            ignore (State_set.add states (state, c) : bool));
      fill ();
      None
    with Done -> Some ()
end
