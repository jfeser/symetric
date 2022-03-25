open Std

type 'a obj = {
  value : 'a;
  mutable parent_dist : float; [@default Float.nan] [@sexp_drop_default.compare]
  mutable radius : float; [@default 0.] [@sexp_drop_default.compare]
  mutable tree : 'a node; [@default Nil] [@sexp_drop_default.sexp]
}

and 'a opaque_obj = 'a obj = {
  value : 'a;
  mutable parent_dist : float; [@default Float.nan] [@sexp_drop_default.compare]
  mutable radius : float; [@default 0.] [@sexp_drop_default.compare]
  mutable tree : ('a node[@sexp.opaque]);
}

and 'a node =
  | Nil
  | Node of {
      mutable objects : 'a obj list; [@sexp.list]
      mutable parent : ('a opaque_obj * ('a node[@sexp.opaque])) option; [@sexp.option]
    }
[@@deriving sexp]

type 'a t = {
  mutable root : 'a node;
  capacity : int;
  distance : 'a -> 'a -> float;
  compare : 'a -> 'a -> int;
  sexp_of : 'a -> Sexp.t;
}
[@@deriving sexp]

let rec iter_obj obj f =
  match obj.tree with Nil -> f obj.value | Node x -> iter_node (Node x) f

and iter_node node f =
  match node with
  | Node { objects; _ } -> List.iter objects ~f:(fun x -> iter_obj x f)
  | Nil -> ()

let iter t f = iter_node t.root f

let invariant t =
  let sexp_of_a = t.sexp_of in
  let open Poly in
  assert (t.capacity >= 0);
  let rec node parent = function
    | Nil -> ()
    | Node x as this ->
        (* every node but the root has a parent *)
        assert (Option.is_some x.parent = Option.is_some parent);

        (* the parent router object points to the current node *)
        (match x.parent with Some (o, _) -> assert (phys_equal o.tree this) | _ -> ());

        (* the parent router object is in the parent node *)
        (match x.parent with
        | Some (o, Node x) -> assert (List.exists x.objects ~f:(phys_equal o))
        | _ -> ());

        (* cached parent distances are correct *)
        List.iter x.objects ~f:(fun obj ->
            Option.iter parent ~f:(fun (pobj, _) ->
                [%test_result: float]
                  ~expect:(t.distance obj.value pobj.value)
                  obj.parent_dist));

        (* router radii are large enough *)
        List.iter x.objects ~f:(fun obj ->
            match obj.tree with
            | Nil -> ()
            | Node _ ->
                iter_node obj.tree (fun v ->
                    let d = t.distance obj.value v in
                    if obj.radius <. d then
                      raise_s
                        [%message
                          "radius too small"
                            (obj.value : a)
                            (v : a)
                            (obj.radius : float)
                            (d : float)]));

        List.iter x.objects ~f:(fun obj -> node (Some (obj, this)) obj.tree)
  in
  node None t.root

let empty ?(capacity = 4) sexp_of compare distance =
  { root = Node { objects = []; parent = None }; distance; capacity; compare; sexp_of }

let mk_obj ?(parent_dist = Float.nan) ?(radius = 0.) ?(tree = Nil) v =
  { value = v; parent_dist; radius; tree }

let parent = function Nil -> None | Node x -> x.parent
let set_parent p = function Nil -> () | Node x -> x.parent <- Some p

let set_covering_radius r =
  match r.tree with
  | Node { objects; _ } ->
      r.radius <-
        Iter.of_list objects
        |> Iter.map (fun obj -> obj.parent_dist +. obj.radius)
        |> Iter.max ~lt:( <. )
        |> Option.value ~default:Float.nan
  | Nil -> ()

let mk_node ?parent objects =
  let node = Node { objects; parent } in
  List.iter objects ~f:(fun router ->
      set_parent (router, node) router.tree;
      set_covering_radius router);
  node

let entries = function Nil -> Iter.empty | Node x -> Iter.of_list x.objects

let is_leaf = function
  | Nil -> false
  | Node x -> List.for_all x.objects ~f:(fun o -> Poly.(o.tree = Nil))

let promote entries =
  let ret = Iter.sample 2 entries in
  let r = ret.(0) and r' = ret.(1) in
  (mk_obj r.value, mk_obj r'.value)

let partition distance (r : 'a obj) (r' : 'a obj) objs =
  let l, l' =
    List.partition_map objs ~f:(fun v ->
        let left_dist = distance v.value r.value in
        let right_dist = distance v.value r'.value in
        let left_closer = left_dist <=. right_dist in
        v.parent_dist <- (if left_closer then left_dist else right_dist);
        if left_closer then First v else Second v)
  in
  r.tree <- mk_node l;
  r'.tree <- mk_node l'

let abs_dist x x' = Float.abs (x -. x')

let%expect_test "" =
  let o1 = mk_obj 1. and o2 = mk_obj 2. and o3 = mk_obj 1.5 in
  let entries = [ o1; o2; o3 ] in
  let r, r' = promote @@ Iter.of_list entries in
  partition abs_dist r r' entries;
  print_s
    [%message
      (r : float obj) (r' : float obj) (o1 : float obj) (o2 : float obj) (o3 : float obj)];
  [%expect
    {|
    ((r
      ((value 1.5)
       (tree
        (Node
         (objects (((value 1) (parent_dist 0.5)) ((value 1.5) (parent_dist 0))))))))
     (r' ((value 2) (tree (Node (objects (((value 2) (parent_dist 0))))))))
     (o1 ((value 1) (parent_dist 0.5))) (o2 ((value 2) (parent_dist 0)))
     (o3 ((value 1.5) (parent_dist 0)))) |}]

let split_open split t split_node v =
  let r, r' = promote @@ Iter.cons v @@ entries split_node in
  partition t.distance r r' (v :: Iter.to_list (entries split_node));

  match parent split_node with
  | Some (parent_router, (Node x as parent_node)) ->
      let gparent_dist v =
        match parent parent_node with
        | Some (gparent_router, _) -> t.distance gparent_router.value v
        | None -> Float.nan
      in

      (* replace this node's router with one of the new routers *)
      x.objects <-
        List.map x.objects ~f:(fun r_old ->
            if t.compare r_old.value parent_router.value = 0 then r else r_old);
      (* the new router is going in the parent, so its new parent is the grandparent *)
      r.parent_dist <- gparent_dist r.value;
      set_parent (r, parent_node) r.tree;
      set_covering_radius r;

      (* insert the other router if possible, otherwise split *)
      if List.length x.objects < t.capacity then (
        x.objects <- r' :: x.objects;
        r'.parent_dist <- gparent_dist r'.value;
        set_parent (r', parent_node) r'.tree;
        set_covering_radius r')
      else split t parent_node r'
  | None ->
      (* the split node was the root, so create a new root *)
      r.parent_dist <- Float.nan;
      r'.parent_dist <- Float.nan;
      set_covering_radius r;
      set_covering_radius r';
      let root = mk_node [ r; r' ] in
      t.root <- root
  | Some (_, Nil) -> assert false

let rec split t node v = split_open split t node v

let%expect_test "" =
  let tree =
    {
      root = Nil;
      distance = abs_dist;
      capacity = 2;
      compare = [%compare: float];
      sexp_of = [%sexp_of: float];
    }
  in
  let rec split t = split_open split t in
  split tree (Node { objects = [ mk_obj 1.; mk_obj 2. ]; parent = None }) (mk_obj 1.5);
  print_s [%message (tree : float t)];
  [%expect
    {|
    (tree
     ((root
       (Node
        (objects
         (((value 2) (radius 0.5)
           (tree
            (Node
             (objects
              (((value 1.5) (parent_dist 0.5)) ((value 2) (parent_dist 0))))
             (parent (((value 2) (radius 0.5) (tree <opaque>)) <opaque>)))))
          ((value 1)
           (tree
            (Node (objects (((value 1) (parent_dist 0))))
             (parent (((value 1) (tree <opaque>)) <opaque>)))))))))
      (capacity 2) (distance <fun>) (compare <fun>) (sexp_of <fun>))) |}]

let insert split t v =
  let rec insert_node parent node =
    match node with
    | Node ({ objects } as l) when List.length objects < t.capacity && is_leaf node ->
        let parent_dist =
          Option.value_map parent ~default:Float.nan ~f:(fun (r, _) ->
              t.distance v r.value)
        in
        let obj = { value = v; parent_dist; tree = Nil; radius = 0. } in
        l.objects <- obj :: objects
    | Node x when List.length x.objects >= t.capacity && is_leaf node ->
        split t node { value = v; parent_dist = Float.nan; radius = 0.; tree = Nil }
    | Node x ->
        let distances =
          List.map x.objects ~f:(fun r -> (t.distance r.value v, r)) |> Iter.of_list
        in
        let closest_covering =
          Iter.filter (fun (d, r) -> d <=. r.radius) distances
          |> Iter.min ~lt:(fun (d, _) (d', _) -> d <. d')
        in
        let insert_here =
          match closest_covering with
          | Some (_, obj) -> obj
          | None ->
              (* no router covers this value, so minimize the increase in covering radius *)
              let dist, obj =
                Iter.min_exn distances ~lt:(fun (d, r) (d', r') ->
                    d -. r.radius <. d' -. r'.radius)
              in

              obj.radius <- dist;
              obj
        in
        insert_node (Some (insert_here, node)) insert_here.tree
    | Nil -> assert false
  in
  insert_node None t.root

let range t v r f =
  let rec range_node query_parent_dist = function
    | Node x ->
        List.iter x.objects ~f:(fun obj ->
            if
              Float.is_nan obj.parent_dist
              || Float.is_nan query_parent_dist
              || Float.abs (query_parent_dist -. obj.parent_dist) <=. r +. obj.radius
            then
              let query_dist = t.distance obj.value v in
              if query_dist <=. r +. obj.radius then
                match obj.tree with
                | Nil -> f obj.value
                | Node _ as node -> range_node query_dist node)
    | Nil -> ()
  in
  range_node Float.nan t.root

let points =
  [
    0.1;
    0.2;
    0.15;
    0.5;
    0.6;
    0.9;
    0.95;
    0.8;
    0.65;
    0.01;
    0.21;
    0.23;
    0.88;
    0.74;
    0.62;
    0.55;
    0.32;
    0.12;
    0.14;
  ]

let%expect_test "" =
  let tree = empty [%sexp_of: float] [%compare: float] abs_dist in
  List.iter points ~f:(fun p -> insert split tree p);
  let range_spec ps v r =
    Iter.of_list ps
    |> Iter.filter (fun v' -> abs_dist v v' <=. r)
    |> Iter.sort ~cmp:[%compare: float]
    |> Iter.to_list
  in
  let range_test t v r =
    range t v r |> Iter.sort ~cmp:[%compare: float] |> Iter.to_list
  in
  List.iter points ~f:(fun p ->
      [%test_result: float * float list]
        ~expect:(p, range_spec points p 0.1)
        (p, range_test tree p 0.1))

let%expect_test "" =
  let tree = empty [%sexp_of: float] [%compare: float] abs_dist in
  List.iteri points ~f:(fun i p ->
      insert split tree p;
      assert (Iter.length @@ iter tree = i + 1);
      print_s [%message (p : float) (tree : float t)];
      invariant tree);
  [%expect
    {|
    ((p 0.1)
     (tree
      ((root (Node (objects (((value 0.1)))))) (capacity 4) (distance <fun>)
       (compare <fun>) (sexp_of <fun>))))
    ((p 0.2)
     (tree
      ((root (Node (objects (((value 0.2)) ((value 0.1)))))) (capacity 4)
       (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.15)
     (tree
      ((root (Node (objects (((value 0.15)) ((value 0.2)) ((value 0.1))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.5)
     (tree
      ((root
        (Node
         (objects (((value 0.5)) ((value 0.15)) ((value 0.2)) ((value 0.1))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.6)
     (tree
      ((root
        (Node
         (objects
          (((value 0.1) (radius 0.1)
            (tree
             (Node
              (objects
               (((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.2) (parent_dist 0.1)) ((value 0.1) (parent_dist 0))))
              (parent (((value 0.1) (radius 0.1) (tree <opaque>)) <opaque>)))))
           ((value 0.5) (radius 0.099999999999999978)
            (tree
             (Node
              (objects
               (((value 0.6) (parent_dist 0.099999999999999978))
                ((value 0.5) (parent_dist 0))))
              (parent
               (((value 0.5) (radius 0.099999999999999978) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.9)
     (tree
      ((root
        (Node
         (objects
          (((value 0.1) (radius 0.1)
            (tree
             (Node
              (objects
               (((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.2) (parent_dist 0.1)) ((value 0.1) (parent_dist 0))))
              (parent (((value 0.1) (radius 0.1) (tree <opaque>)) <opaque>)))))
           ((value 0.5) (radius 0.4)
            (tree
             (Node
              (objects
               (((value 0.9) (parent_dist 0.4))
                ((value 0.6) (parent_dist 0.099999999999999978))
                ((value 0.5) (parent_dist 0))))
              (parent (((value 0.5) (radius 0.4) (tree <opaque>)) <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.95)
     (tree
      ((root
        (Node
         (objects
          (((value 0.1) (radius 0.1)
            (tree
             (Node
              (objects
               (((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.2) (parent_dist 0.1)) ((value 0.1) (parent_dist 0))))
              (parent (((value 0.1) (radius 0.1) (tree <opaque>)) <opaque>)))))
           ((value 0.5) (radius 0.44999999999999996)
            (tree
             (Node
              (objects
               (((value 0.95) (parent_dist 0.44999999999999996))
                ((value 0.9) (parent_dist 0.4))
                ((value 0.6) (parent_dist 0.099999999999999978))
                ((value 0.5) (parent_dist 0))))
              (parent
               (((value 0.5) (radius 0.44999999999999996) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.8)
     (tree
      ((root
        (Node
         (objects
          (((value 0.9) (radius 0.049999999999999933)
            (tree
             (Node
              (objects
               (((value 0.95) (parent_dist 0.049999999999999933))
                ((value 0.9) (parent_dist 0))))
              (parent
               (((value 0.9) (radius 0.049999999999999933) (tree <opaque>))
                <opaque>)))))
           ((value 0.1) (radius 0.1)
            (tree
             (Node
              (objects
               (((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.2) (parent_dist 0.1)) ((value 0.1) (parent_dist 0))))
              (parent (((value 0.1) (radius 0.1) (tree <opaque>)) <opaque>)))))
           ((value 0.8) (radius 0.30000000000000004)
            (tree
             (Node
              (objects
               (((value 0.8) (parent_dist 0))
                ((value 0.6) (parent_dist 0.20000000000000007))
                ((value 0.5) (parent_dist 0.30000000000000004))))
              (parent
               (((value 0.8) (radius 0.30000000000000004) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.65)
     (tree
      ((root
        (Node
         (objects
          (((value 0.9) (radius 0.049999999999999933)
            (tree
             (Node
              (objects
               (((value 0.95) (parent_dist 0.049999999999999933))
                ((value 0.9) (parent_dist 0))))
              (parent
               (((value 0.9) (radius 0.049999999999999933) (tree <opaque>))
                <opaque>)))))
           ((value 0.1) (radius 0.1)
            (tree
             (Node
              (objects
               (((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.2) (parent_dist 0.1)) ((value 0.1) (parent_dist 0))))
              (parent (((value 0.1) (radius 0.1) (tree <opaque>)) <opaque>)))))
           ((value 0.8) (radius 0.30000000000000004)
            (tree
             (Node
              (objects
               (((value 0.65) (parent_dist 0.15000000000000002))
                ((value 0.8) (parent_dist 0))
                ((value 0.6) (parent_dist 0.20000000000000007))
                ((value 0.5) (parent_dist 0.30000000000000004))))
              (parent
               (((value 0.8) (radius 0.30000000000000004) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.01)
     (tree
      ((root
        (Node
         (objects
          (((value 0.9) (radius 0.049999999999999933)
            (tree
             (Node
              (objects
               (((value 0.95) (parent_dist 0.049999999999999933))
                ((value 0.9) (parent_dist 0))))
              (parent
               (((value 0.9) (radius 0.049999999999999933) (tree <opaque>))
                <opaque>)))))
           ((value 0.1) (radius 0.1)
            (tree
             (Node
              (objects
               (((value 0.01) (parent_dist 0.090000000000000011))
                ((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.2) (parent_dist 0.1)) ((value 0.1) (parent_dist 0))))
              (parent (((value 0.1) (radius 0.1) (tree <opaque>)) <opaque>)))))
           ((value 0.8) (radius 0.30000000000000004)
            (tree
             (Node
              (objects
               (((value 0.65) (parent_dist 0.15000000000000002))
                ((value 0.8) (parent_dist 0))
                ((value 0.6) (parent_dist 0.20000000000000007))
                ((value 0.5) (parent_dist 0.30000000000000004))))
              (parent
               (((value 0.8) (radius 0.30000000000000004) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.21)
     (tree
      ((root
        (Node
         (objects
          (((value 0.1) (radius 0.090000000000000011)
            (tree
             (Node
              (objects
               (((value 0.01) (parent_dist 0.090000000000000011))
                ((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.1) (parent_dist 0))))
              (parent
               (((value 0.1) (radius 0.090000000000000011) (tree <opaque>))
                <opaque>)))))
           ((value 0.9) (radius 0.049999999999999933)
            (tree
             (Node
              (objects
               (((value 0.95) (parent_dist 0.049999999999999933))
                ((value 0.9) (parent_dist 0))))
              (parent
               (((value 0.9) (radius 0.049999999999999933) (tree <opaque>))
                <opaque>)))))
           ((value 0.21) (radius 0.0099999999999999811)
            (tree
             (Node
              (objects
               (((value 0.21) (parent_dist 0))
                ((value 0.2) (parent_dist 0.0099999999999999811))))
              (parent
               (((value 0.21) (radius 0.0099999999999999811) (tree <opaque>))
                <opaque>)))))
           ((value 0.8) (radius 0.30000000000000004)
            (tree
             (Node
              (objects
               (((value 0.65) (parent_dist 0.15000000000000002))
                ((value 0.8) (parent_dist 0))
                ((value 0.6) (parent_dist 0.20000000000000007))
                ((value 0.5) (parent_dist 0.30000000000000004))))
              (parent
               (((value 0.8) (radius 0.30000000000000004) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.23)
     (tree
      ((root
        (Node
         (objects
          (((value 0.1) (radius 0.090000000000000011)
            (tree
             (Node
              (objects
               (((value 0.01) (parent_dist 0.090000000000000011))
                ((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.1) (parent_dist 0))))
              (parent
               (((value 0.1) (radius 0.090000000000000011) (tree <opaque>))
                <opaque>)))))
           ((value 0.9) (radius 0.049999999999999933)
            (tree
             (Node
              (objects
               (((value 0.95) (parent_dist 0.049999999999999933))
                ((value 0.9) (parent_dist 0))))
              (parent
               (((value 0.9) (radius 0.049999999999999933) (tree <opaque>))
                <opaque>)))))
           ((value 0.21) (radius 0.020000000000000018)
            (tree
             (Node
              (objects
               (((value 0.23) (parent_dist 0.020000000000000018))
                ((value 0.21) (parent_dist 0))
                ((value 0.2) (parent_dist 0.0099999999999999811))))
              (parent
               (((value 0.21) (radius 0.020000000000000018) (tree <opaque>))
                <opaque>)))))
           ((value 0.8) (radius 0.30000000000000004)
            (tree
             (Node
              (objects
               (((value 0.65) (parent_dist 0.15000000000000002))
                ((value 0.8) (parent_dist 0))
                ((value 0.6) (parent_dist 0.20000000000000007))
                ((value 0.5) (parent_dist 0.30000000000000004))))
              (parent
               (((value 0.8) (radius 0.30000000000000004) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.88)
     (tree
      ((root
        (Node
         (objects
          (((value 0.1) (radius 0.090000000000000011)
            (tree
             (Node
              (objects
               (((value 0.01) (parent_dist 0.090000000000000011))
                ((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.1) (parent_dist 0))))
              (parent
               (((value 0.1) (radius 0.090000000000000011) (tree <opaque>))
                <opaque>)))))
           ((value 0.9) (radius 0.049999999999999933)
            (tree
             (Node
              (objects
               (((value 0.88) (parent_dist 0.020000000000000018))
                ((value 0.95) (parent_dist 0.049999999999999933))
                ((value 0.9) (parent_dist 0))))
              (parent
               (((value 0.9) (radius 0.049999999999999933) (tree <opaque>))
                <opaque>)))))
           ((value 0.21) (radius 0.020000000000000018)
            (tree
             (Node
              (objects
               (((value 0.23) (parent_dist 0.020000000000000018))
                ((value 0.21) (parent_dist 0))
                ((value 0.2) (parent_dist 0.0099999999999999811))))
              (parent
               (((value 0.21) (radius 0.020000000000000018) (tree <opaque>))
                <opaque>)))))
           ((value 0.8) (radius 0.30000000000000004)
            (tree
             (Node
              (objects
               (((value 0.65) (parent_dist 0.15000000000000002))
                ((value 0.8) (parent_dist 0))
                ((value 0.6) (parent_dist 0.20000000000000007))
                ((value 0.5) (parent_dist 0.30000000000000004))))
              (parent
               (((value 0.8) (radius 0.30000000000000004) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.74)
     (tree
      ((root
        (Node
         (objects
          (((value 0.21) (radius 0.2)
            (tree
             (Node
              (objects
               (((value 0.1) (parent_dist 0.10999999999999999)
                 (radius 0.090000000000000011)
                 (tree
                  (Node
                   (objects
                    (((value 0.01) (parent_dist 0.090000000000000011))
                     ((value 0.15) (parent_dist 0.049999999999999989))
                     ((value 0.1) (parent_dist 0))))
                   (parent
                    (((value 0.1) (parent_dist 0.10999999999999999)
                      (radius 0.090000000000000011) (tree <opaque>))
                     <opaque>)))))
                ((value 0.21) (parent_dist 0) (radius 0.020000000000000018)
                 (tree
                  (Node
                   (objects
                    (((value 0.23) (parent_dist 0.020000000000000018))
                     ((value 0.21) (parent_dist 0))
                     ((value 0.2) (parent_dist 0.0099999999999999811))))
                   (parent
                    (((value 0.21) (parent_dist 0) (radius 0.020000000000000018)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.21) (radius 0.2) (tree <opaque>)) <opaque>)))))
           ((value 0.6) (radius 0.35)
            (tree
             (Node
              (objects
               (((value 0.65) (parent_dist 0.050000000000000044)
                 (radius 0.15000000000000002)
                 (tree
                  (Node
                   (objects
                    (((value 0.74) (parent_dist 0.089999999999999969))
                     ((value 0.65) (parent_dist 0))
                     ((value 0.8) (parent_dist 0.15000000000000002))))
                   (parent
                    (((value 0.65) (parent_dist 0.050000000000000044)
                      (radius 0.15000000000000002) (tree <opaque>))
                     <opaque>)))))
                ((value 0.9) (parent_dist 0.30000000000000004)
                 (radius 0.049999999999999933)
                 (tree
                  (Node
                   (objects
                    (((value 0.88) (parent_dist 0.020000000000000018))
                     ((value 0.95) (parent_dist 0.049999999999999933))
                     ((value 0.9) (parent_dist 0))))
                   (parent
                    (((value 0.9) (parent_dist 0.30000000000000004)
                      (radius 0.049999999999999933) (tree <opaque>))
                     <opaque>)))))
                ((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                 (tree
                  (Node
                   (objects
                    (((value 0.6) (parent_dist 0))
                     ((value 0.5) (parent_dist 0.099999999999999978))))
                   (parent
                    (((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.6) (radius 0.35) (tree <opaque>)) <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.62)
     (tree
      ((root
        (Node
         (objects
          (((value 0.21) (radius 0.2)
            (tree
             (Node
              (objects
               (((value 0.1) (parent_dist 0.10999999999999999)
                 (radius 0.090000000000000011)
                 (tree
                  (Node
                   (objects
                    (((value 0.01) (parent_dist 0.090000000000000011))
                     ((value 0.15) (parent_dist 0.049999999999999989))
                     ((value 0.1) (parent_dist 0))))
                   (parent
                    (((value 0.1) (parent_dist 0.10999999999999999)
                      (radius 0.090000000000000011) (tree <opaque>))
                     <opaque>)))))
                ((value 0.21) (parent_dist 0) (radius 0.020000000000000018)
                 (tree
                  (Node
                   (objects
                    (((value 0.23) (parent_dist 0.020000000000000018))
                     ((value 0.21) (parent_dist 0))
                     ((value 0.2) (parent_dist 0.0099999999999999811))))
                   (parent
                    (((value 0.21) (parent_dist 0) (radius 0.020000000000000018)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.21) (radius 0.2) (tree <opaque>)) <opaque>)))))
           ((value 0.6) (radius 0.35)
            (tree
             (Node
              (objects
               (((value 0.65) (parent_dist 0.050000000000000044)
                 (radius 0.15000000000000002)
                 (tree
                  (Node
                   (objects
                    (((value 0.74) (parent_dist 0.089999999999999969))
                     ((value 0.65) (parent_dist 0))
                     ((value 0.8) (parent_dist 0.15000000000000002))))
                   (parent
                    (((value 0.65) (parent_dist 0.050000000000000044)
                      (radius 0.15000000000000002) (tree <opaque>))
                     <opaque>)))))
                ((value 0.9) (parent_dist 0.30000000000000004)
                 (radius 0.049999999999999933)
                 (tree
                  (Node
                   (objects
                    (((value 0.88) (parent_dist 0.020000000000000018))
                     ((value 0.95) (parent_dist 0.049999999999999933))
                     ((value 0.9) (parent_dist 0))))
                   (parent
                    (((value 0.9) (parent_dist 0.30000000000000004)
                      (radius 0.049999999999999933) (tree <opaque>))
                     <opaque>)))))
                ((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                 (tree
                  (Node
                   (objects
                    (((value 0.62) (parent_dist 0.020000000000000018))
                     ((value 0.6) (parent_dist 0))
                     ((value 0.5) (parent_dist 0.099999999999999978))))
                   (parent
                    (((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.6) (radius 0.35) (tree <opaque>)) <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.55)
     (tree
      ((root
        (Node
         (objects
          (((value 0.21) (radius 0.2)
            (tree
             (Node
              (objects
               (((value 0.1) (parent_dist 0.10999999999999999)
                 (radius 0.090000000000000011)
                 (tree
                  (Node
                   (objects
                    (((value 0.01) (parent_dist 0.090000000000000011))
                     ((value 0.15) (parent_dist 0.049999999999999989))
                     ((value 0.1) (parent_dist 0))))
                   (parent
                    (((value 0.1) (parent_dist 0.10999999999999999)
                      (radius 0.090000000000000011) (tree <opaque>))
                     <opaque>)))))
                ((value 0.21) (parent_dist 0) (radius 0.020000000000000018)
                 (tree
                  (Node
                   (objects
                    (((value 0.23) (parent_dist 0.020000000000000018))
                     ((value 0.21) (parent_dist 0))
                     ((value 0.2) (parent_dist 0.0099999999999999811))))
                   (parent
                    (((value 0.21) (parent_dist 0) (radius 0.020000000000000018)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.21) (radius 0.2) (tree <opaque>)) <opaque>)))))
           ((value 0.6) (radius 0.35)
            (tree
             (Node
              (objects
               (((value 0.65) (parent_dist 0.050000000000000044)
                 (radius 0.15000000000000002)
                 (tree
                  (Node
                   (objects
                    (((value 0.74) (parent_dist 0.089999999999999969))
                     ((value 0.65) (parent_dist 0))
                     ((value 0.8) (parent_dist 0.15000000000000002))))
                   (parent
                    (((value 0.65) (parent_dist 0.050000000000000044)
                      (radius 0.15000000000000002) (tree <opaque>))
                     <opaque>)))))
                ((value 0.9) (parent_dist 0.30000000000000004)
                 (radius 0.049999999999999933)
                 (tree
                  (Node
                   (objects
                    (((value 0.88) (parent_dist 0.020000000000000018))
                     ((value 0.95) (parent_dist 0.049999999999999933))
                     ((value 0.9) (parent_dist 0))))
                   (parent
                    (((value 0.9) (parent_dist 0.30000000000000004)
                      (radius 0.049999999999999933) (tree <opaque>))
                     <opaque>)))))
                ((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                 (tree
                  (Node
                   (objects
                    (((value 0.55) (parent_dist 0.049999999999999933))
                     ((value 0.62) (parent_dist 0.020000000000000018))
                     ((value 0.6) (parent_dist 0))
                     ((value 0.5) (parent_dist 0.099999999999999978))))
                   (parent
                    (((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.6) (radius 0.35) (tree <opaque>)) <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.32)
     (tree
      ((root
        (Node
         (objects
          (((value 0.21) (radius 0.2)
            (tree
             (Node
              (objects
               (((value 0.1) (parent_dist 0.10999999999999999)
                 (radius 0.090000000000000011)
                 (tree
                  (Node
                   (objects
                    (((value 0.01) (parent_dist 0.090000000000000011))
                     ((value 0.15) (parent_dist 0.049999999999999989))
                     ((value 0.1) (parent_dist 0))))
                   (parent
                    (((value 0.1) (parent_dist 0.10999999999999999)
                      (radius 0.090000000000000011) (tree <opaque>))
                     <opaque>)))))
                ((value 0.21) (parent_dist 0) (radius 0.11000000000000001)
                 (tree
                  (Node
                   (objects
                    (((value 0.32) (parent_dist 0.11000000000000001))
                     ((value 0.23) (parent_dist 0.020000000000000018))
                     ((value 0.21) (parent_dist 0))
                     ((value 0.2) (parent_dist 0.0099999999999999811))))
                   (parent
                    (((value 0.21) (parent_dist 0) (radius 0.11000000000000001)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.21) (radius 0.2) (tree <opaque>)) <opaque>)))))
           ((value 0.6) (radius 0.35)
            (tree
             (Node
              (objects
               (((value 0.65) (parent_dist 0.050000000000000044)
                 (radius 0.15000000000000002)
                 (tree
                  (Node
                   (objects
                    (((value 0.74) (parent_dist 0.089999999999999969))
                     ((value 0.65) (parent_dist 0))
                     ((value 0.8) (parent_dist 0.15000000000000002))))
                   (parent
                    (((value 0.65) (parent_dist 0.050000000000000044)
                      (radius 0.15000000000000002) (tree <opaque>))
                     <opaque>)))))
                ((value 0.9) (parent_dist 0.30000000000000004)
                 (radius 0.049999999999999933)
                 (tree
                  (Node
                   (objects
                    (((value 0.88) (parent_dist 0.020000000000000018))
                     ((value 0.95) (parent_dist 0.049999999999999933))
                     ((value 0.9) (parent_dist 0))))
                   (parent
                    (((value 0.9) (parent_dist 0.30000000000000004)
                      (radius 0.049999999999999933) (tree <opaque>))
                     <opaque>)))))
                ((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                 (tree
                  (Node
                   (objects
                    (((value 0.55) (parent_dist 0.049999999999999933))
                     ((value 0.62) (parent_dist 0.020000000000000018))
                     ((value 0.6) (parent_dist 0))
                     ((value 0.5) (parent_dist 0.099999999999999978))))
                   (parent
                    (((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.6) (radius 0.35) (tree <opaque>)) <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.12)
     (tree
      ((root
        (Node
         (objects
          (((value 0.21) (radius 0.2)
            (tree
             (Node
              (objects
               (((value 0.1) (parent_dist 0.10999999999999999)
                 (radius 0.090000000000000011)
                 (tree
                  (Node
                   (objects
                    (((value 0.12) (parent_dist 0.01999999999999999))
                     ((value 0.01) (parent_dist 0.090000000000000011))
                     ((value 0.15) (parent_dist 0.049999999999999989))
                     ((value 0.1) (parent_dist 0))))
                   (parent
                    (((value 0.1) (parent_dist 0.10999999999999999)
                      (radius 0.090000000000000011) (tree <opaque>))
                     <opaque>)))))
                ((value 0.21) (parent_dist 0) (radius 0.11000000000000001)
                 (tree
                  (Node
                   (objects
                    (((value 0.32) (parent_dist 0.11000000000000001))
                     ((value 0.23) (parent_dist 0.020000000000000018))
                     ((value 0.21) (parent_dist 0))
                     ((value 0.2) (parent_dist 0.0099999999999999811))))
                   (parent
                    (((value 0.21) (parent_dist 0) (radius 0.11000000000000001)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.21) (radius 0.2) (tree <opaque>)) <opaque>)))))
           ((value 0.6) (radius 0.35)
            (tree
             (Node
              (objects
               (((value 0.65) (parent_dist 0.050000000000000044)
                 (radius 0.15000000000000002)
                 (tree
                  (Node
                   (objects
                    (((value 0.74) (parent_dist 0.089999999999999969))
                     ((value 0.65) (parent_dist 0))
                     ((value 0.8) (parent_dist 0.15000000000000002))))
                   (parent
                    (((value 0.65) (parent_dist 0.050000000000000044)
                      (radius 0.15000000000000002) (tree <opaque>))
                     <opaque>)))))
                ((value 0.9) (parent_dist 0.30000000000000004)
                 (radius 0.049999999999999933)
                 (tree
                  (Node
                   (objects
                    (((value 0.88) (parent_dist 0.020000000000000018))
                     ((value 0.95) (parent_dist 0.049999999999999933))
                     ((value 0.9) (parent_dist 0))))
                   (parent
                    (((value 0.9) (parent_dist 0.30000000000000004)
                      (radius 0.049999999999999933) (tree <opaque>))
                     <opaque>)))))
                ((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                 (tree
                  (Node
                   (objects
                    (((value 0.55) (parent_dist 0.049999999999999933))
                     ((value 0.62) (parent_dist 0.020000000000000018))
                     ((value 0.6) (parent_dist 0))
                     ((value 0.5) (parent_dist 0.099999999999999978))))
                   (parent
                    (((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.6) (radius 0.35) (tree <opaque>)) <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>))))
    ((p 0.14)
     (tree
      ((root
        (Node
         (objects
          (((value 0.21) (radius 0.2)
            (tree
             (Node
              (objects
               (((value 0.15) (parent_dist 0.06)
                 (tree
                  (Node (objects (((value 0.15) (parent_dist 0))))
                   (parent
                    (((value 0.15) (parent_dist 0.06) (tree <opaque>)) <opaque>)))))
                ((value 0.14) (parent_dist 0.069999999999999979) (radius 0.13)
                 (tree
                  (Node
                   (objects
                    (((value 0.14) (parent_dist 0))
                     ((value 0.12) (parent_dist 0.020000000000000018))
                     ((value 0.01) (parent_dist 0.13))
                     ((value 0.1) (parent_dist 0.040000000000000008))))
                   (parent
                    (((value 0.14) (parent_dist 0.069999999999999979)
                      (radius 0.13) (tree <opaque>))
                     <opaque>)))))
                ((value 0.21) (parent_dist 0) (radius 0.11000000000000001)
                 (tree
                  (Node
                   (objects
                    (((value 0.32) (parent_dist 0.11000000000000001))
                     ((value 0.23) (parent_dist 0.020000000000000018))
                     ((value 0.21) (parent_dist 0))
                     ((value 0.2) (parent_dist 0.0099999999999999811))))
                   (parent
                    (((value 0.21) (parent_dist 0) (radius 0.11000000000000001)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.21) (radius 0.2) (tree <opaque>)) <opaque>)))))
           ((value 0.6) (radius 0.35)
            (tree
             (Node
              (objects
               (((value 0.65) (parent_dist 0.050000000000000044)
                 (radius 0.15000000000000002)
                 (tree
                  (Node
                   (objects
                    (((value 0.74) (parent_dist 0.089999999999999969))
                     ((value 0.65) (parent_dist 0))
                     ((value 0.8) (parent_dist 0.15000000000000002))))
                   (parent
                    (((value 0.65) (parent_dist 0.050000000000000044)
                      (radius 0.15000000000000002) (tree <opaque>))
                     <opaque>)))))
                ((value 0.9) (parent_dist 0.30000000000000004)
                 (radius 0.049999999999999933)
                 (tree
                  (Node
                   (objects
                    (((value 0.88) (parent_dist 0.020000000000000018))
                     ((value 0.95) (parent_dist 0.049999999999999933))
                     ((value 0.9) (parent_dist 0))))
                   (parent
                    (((value 0.9) (parent_dist 0.30000000000000004)
                      (radius 0.049999999999999933) (tree <opaque>))
                     <opaque>)))))
                ((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                 (tree
                  (Node
                   (objects
                    (((value 0.55) (parent_dist 0.049999999999999933))
                     ((value 0.62) (parent_dist 0.020000000000000018))
                     ((value 0.6) (parent_dist 0))
                     ((value 0.5) (parent_dist 0.099999999999999978))))
                   (parent
                    (((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.6) (radius 0.35) (tree <opaque>)) <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>) (sexp_of <fun>)))) |}]

let%test_unit "" =
  let tree = empty [%sexp_of: float] [%compare: float] abs_dist in
  for _ = 0 to 1000 do
    insert split tree (Random.float 1.);
    invariant tree
  done
