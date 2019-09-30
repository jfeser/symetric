open! Core
open Ppx_stage

module Value = struct
  type t =
    | A of int array code
    | I of int code
    | F_int of (int code -> int code)
    | F_bool of (int code -> bool code)
    | F_int2 of (int code -> int code -> int code)
end

module Lang = struct
  let grammar : Grammar.t =
    let open Grammar in
    let open Grammar.Term in
    [
      ("I", Id "k");
      ("L", Id "b");
      ("I", App ("head", [ Id "L" ]));
      ("I", App ("last", [ Id "L" ]));
      ("L", App ("take", [ Id "I"; Id "L" ]));
      ("L", App ("drop", [ Id "I"; Id "L" ]));
      ("I", App ("access", [ Id "I"; Id "L" ]));
      ("I", App ("minimum", [ Id "L" ]));
      ("I", App ("maximum", [ Id "L" ]));
      ("L", App ("reverse", [ Id "L" ]));
      ("L", App ("sort", [ Id "L" ]));
      ("I", App ("sum", [ Id "L" ]));
      ("L", App ("map", [ Id "FII"; Id "L" ]));
      ("L", App ("filter", [ Id "FIB"; Id "L" ]));
      ("I", App ("count", [ Id "FIB"; Id "L" ]));
      ("L", App ("zipwith", [ Id "FIII"; Id "L"; Id "L" ]));
      ("L", App ("scanl1", [ Id "FIII"; Id "L" ]));
      ("FII", Id "(+1)");
      ("FII", Id "(-1)");
      ("FII", Id "(*2)");
      ("FII", Id "(/2)");
      ("FII", Id "(*(-1))");
      ("FII", Id "(**2)");
      ("FII", Id "(*3)");
      ("FII", Id "(/3)");
      ("FII", Id "(*4)");
      ("FII", Id "(/4)");
      ("FIB", Id "(>0)");
      ("FIB", Id "(<0)");
      ("FIB", Id "(%2==0)");
      ("FIB", Id "(%2==1)");
      ("FIII", Id "(+)");
      ("FIII", Id "(-)");
      ("FIII", Id "(*)");
      ("FIII", Id "min");
      ("FIII", Id "max");
    ]
    |> inline "FII" |> inline "FIB" |> inline "FIII"
    |> List.filter ~f:(fun (lhs, _) -> String.(lhs = "L" || lhs = "I"))

  type value = Value.t

  open Value

  let rec eval ctx =
    let open Grammar.Term in
    function
    | Id "k" -> I [%code 2]
    | Id "b" -> A [%code [| 3; 5; 4; 7; 5 |]]
    | Id "(+1)" -> F_int (fun x -> [%code [%e x] + 1])
    | Id "(-1)" -> F_int (fun x -> [%code [%e x] - 1])
    | Id "(*2)" -> F_int (fun x -> [%code [%e x] * 2])
    | Id "(/2)" -> F_int (fun x -> [%code [%e x] / 2])
    | Id "(*(-1))" -> F_int (fun x -> [%code -[%e x]])
    | Id "(**2)" -> F_int (fun x -> [%code [%e x] * [%e x]])
    | Id "(*3)" -> F_int (fun x -> [%code [%e x] * 3])
    | Id "(/3)" -> F_int (fun x -> [%code [%e x] / 3])
    | Id "(*4)" -> F_int (fun x -> [%code [%e x] * 4])
    | Id "(/4)" -> F_int (fun x -> [%code [%e x] / 4])
    | Id "(>0)" -> F_bool (fun x -> [%code [%e x] > 0])
    | Id "(<0)" -> F_bool (fun x -> [%code [%e x] < 0])
    | Id "(%2==0)" -> F_bool (fun x -> [%code [%e x] mod 2 = 0])
    | Id "(%2==1)" -> F_bool (fun x -> [%code [%e x] mod 2 = 1])
    | Id "(+)" -> F_int2 (fun x y -> [%code [%e x] + [%e y]])
    | Id "(-)" -> F_int2 (fun x y -> [%code [%e x] - [%e y]])
    | Id "(*)" -> F_int2 (fun x y -> [%code [%e x] * [%e y]])
    | Id "min" ->
        F_int2 (fun x y -> [%code if [%e x] < [%e y] then [%e x] else [%e y]])
    | Id "max" ->
        F_int2 (fun x y -> [%code if [%e x] < [%e y] then [%e y] else [%e x]])
    | Id x -> Map.find_exn ctx x
    | App ("head", [ e ]) ->
        let (A a) = eval ctx e in
        I [%code [%e a].(0)]
    | App ("last", [ e ]) ->
        let (A a) = eval ctx e in
        I [%code [%e a].(Array.length [%e a] - 1)]
    | App ("take", [ n; e ]) ->
        let (A arr) = eval ctx e in
        let (I n) = eval ctx n in
        A [%code Array.sub [%e arr] 0 [%e n]]
    | App ("drop", [ n; e ]) ->
        let (A arr) = eval ctx e in
        let (I elems) = eval ctx n in
        A
          [%code
            Array.sub [%e arr] [%e elems] (Array.length [%e arr] - [%e elems])]
    | App ("access", [ n; e ]) ->
        let (A a) = eval ctx e in
        let (I n) = eval ctx n in
        I [%code [%e a].([%e n])]
    | App ("minimum", [ e ]) ->
        let (A arr) = eval ctx e in
        I
          [%code
            let min = ref max_int in
            for i = 0 to Array.length [%e arr] - 1 do
              min := if [%e arr].(i) < !min then [%e arr].(i) else !min
            done;
            !min]
    | App ("maximum", [ e ]) ->
        let (A arr) = eval ctx e in
        I
          [%code
            let max = ref min_int in
            for i = 0 to Array.length [%e arr] - 1 do
              max := if [%e arr].(i) > !max then [%e arr].(i) else !max
            done;
            !max]
    | App ("reverse", [ e ]) ->
        let (A arr) = eval ctx e in
        A
          [%code
            let l = Array.length [%e arr] in
            Array.init l (fun i -> [%e arr].(l - i - 1))]
    | App ("sort", [ e ]) ->
        let (A a) = eval ctx e in
        A
          [%code
            let a' = Array.copy [%e a] in
            Array.sort compare a';
            a']
    | App ("sum", [ e ]) ->
        let (A a) = eval ctx e in
        I
          [%code
            let sum = ref 0 in
            for i = 0 to Array.length [%e a] - 1 do
              sum := !sum + [%e a].(i)
            done;
            !sum]
    | App ("map", [ f; e ]) ->
        let (F_int f) = eval ctx f in
        let (A a) = eval ctx e in
        A
          [%code
            let n = Array.length [%e a] in
            let a' = Array.make n 0 in
            for i = 0 to n - 1 do
              a'.(i) <- [%e f [%code a'.(i)]]
            done;
            a']
    | App ("filter", [ f; e ]) ->
        let (F_bool f) = eval ctx f in
        let (A a) = eval ctx e in
        A
          [%code
            let n = Array.length [%e a] in
            let a' = Array.make n 0 in
            let j = ref 0 in
            for i = 0 to n - 1 do
              if [%e f [%code [%e a].(i)]] then (
                a'.(!j) <- [%e a].(i);
                incr j )
            done;
            Array.sub a' 0 (!j + 1)]
    | App ("count", [ f; e ]) ->
        let (F_bool f) = eval ctx f in
        let (A a) = eval ctx e in
        I
          [%code
            let x = ref 0 in
            for i = 0 to Array.length [%e a] - 1 do
              if [%e f [%code [%e a].(i)]] then incr x
            done;
            !x]
    | App ("zipwith", [ f; e; e' ]) ->
        let (F_int2 f) = eval ctx f in
        let (A a) = eval ctx e in
        let (A a') = eval ctx e' in
        A
          [%code
            let n = Array.length [%e a] in
            let a'' = Array.make n 0 in
            for i = 0 to n - 1 do
              a''.(i) <- [%e f [%code [%e a].(i)] [%code [%e a'].(i)]]
            done;
            a'']
    | App ("scanl1", [ f; e ]) ->
        let (F_int2 f) = eval ctx f in
        let (A a) = eval ctx e in
        I
          [%code
            let x = ref [%e a].(0) in
            for i = 1 to Array.length [%e a] - 1 do
              x := [%e f [%code !x] [%code [%e a].(i)]]
            done;
            !x]
    | _ -> assert false
end

module Cache = struct
  open Values

  type value = Value.t

  type t = {
    target : Value.t;
    ints : Int_tuple_set.t array code;
    arrays : Int_array_tuple_set.t array code;
  }

  let empty target (k : t -> 'a code) : 'a code =
    [%code
      let tbl_i = Array.init 100 (fun _ -> Values.Int_tuple_set.create ()) in
      let tbl_a =
        Array.init 100 (fun _ -> Values.Int_array_tuple_set.create ())
      in
      [%e k { target; ints = tbl_i; arrays = tbl_a }]]

  let put ~sym:_ ~size ~sizes { target; ints = tbl_i; arrays = tbl_a; _ } v =
    let key = Lift.int size in
    let sizes = Stage.Lift.(list int) sizes in
    match (v, target) with
    | Value.I v, Value.I v' ->
        [%code
          let v = [%e v] in
          let v' = [%e v'] in
          if v = v' then failwith "Found solution";
          Core.Hash_set.add
            [%e tbl_i].([%e key])
            { Values.Int_tuple_set.Elt.value = v; sizes = [%e sizes] }]
    | Value.I v, _ ->
        [%code
          Core.Hash_set.add
            [%e tbl_i].([%e key])
            { Values.Int_tuple_set.Elt.value = [%e v]; sizes = [%e sizes] }]
    | A v, A v' ->
        [%code
          let v = [%e v] in
          let v' = [%e v'] in
          if v = v' then failwith "Found solution";
          Core.Hash_set.add
            [%e tbl_a].([%e key])
            { Values.Int_array_tuple_set.Elt.value = v; sizes = [%e sizes] }]
    | A v, _ ->
        [%code
          Core.Hash_set.add
            [%e tbl_a].([%e key])
            {
              Values.Int_array_tuple_set.Elt.value = [%e v];
              sizes = [%e sizes];
            }]
    | _ -> assert false

  let iter ~sym ~size ~f { ints = tbl_i; arrays = tbl_a; _ } =
    let key = Lift.int size in
    match sym with
    | "I" ->
        [%code
          Core.Hash_set.iter
            [%e tbl_i].([%e key])
            ~f:(fun v ->
              [%e f (Value.I [%code [%e v].Values.Int_tuple_set.Elt.value])])]
    | "L" ->
        [%code
          Core.Hash_set.iter
            [%e tbl_a].([%e key])
            ~f:(fun v ->
              [%e
                f (Value.A [%code [%e v].Values.Int_array_tuple_set.Elt.value])])]
    | _ -> assert false

  let print_size _tbl = [%code ()]
end
