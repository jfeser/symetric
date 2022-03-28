open Std

[@@@warning "-30"]

type 'a obj = {
  value : 'a;
  mutable parent_dist : float; [@default Float.nan] [@sexp_drop_default.compare]
  mutable radius : float; [@default 0.] [@sexp_drop_default.compare]
  mutable tree : 'a node option; [@sexp.option]
}

and 'a opaque_obj = 'a obj = {
  value : 'a;
  mutable parent_dist : float; [@default Float.nan] [@sexp_drop_default.compare]
  mutable radius : float; [@default 0.] [@sexp_drop_default.compare]
  mutable tree : ('a node option[@sexp.opaque]);
}

and 'a node = {
  mutable objects : 'a obj list; [@sexp.list]
  mutable parent : ('a opaque_obj * ('a node[@sexp.opaque])) option; [@sexp.option]
}
[@@deriving sexp]

[@@@warning "+30"]

type 'a t = {
  mutable root : 'a node;
  capacity : int;
  distance : 'a -> 'a -> float;
  compare : 'a -> 'a -> int;
}
[@@deriving sexp]

let rec iter_obj obj f =
  match obj.tree with None -> f obj.value | Some node -> iter_node node f

and iter_node { objects; _ } f = List.iter objects ~f:(fun x -> iter_obj x f)

let iter t f = iter_node t.root f

let invariant t sexp_of_a =
  let open Poly in
  assert (t.capacity >= 0);
  let rec node parent this =
    (* every node but the root has a parent *)
    assert (Option.is_some this.parent = Option.is_some parent);

    (* the parent router object points to the current node *)
    (match this.parent with
    | Some ({ tree = Some node }, _) -> assert (phys_equal node this)
    | _ -> ());

    (* the parent router object is in the parent node *)
    (match this.parent with
    | Some (o, x) -> assert (List.exists x.objects ~f:(phys_equal o))
    | _ -> ());

    (* cached parent distances are correct *)
    List.iter this.objects ~f:(fun obj ->
        Option.iter parent ~f:(fun (pobj, _) ->
            [%test_result: float]
              ~expect:(t.distance obj.value pobj.value)
              obj.parent_dist));

    (* router radii are large enough *)
    List.iter this.objects ~f:(fun obj ->
        match obj.tree with
        | None -> ()
        | Some node ->
            iter_node node (fun v ->
                let d = t.distance obj.value v in
                if obj.radius <. d then
                  raise_s
                    [%message
                      "radius too small"
                        (obj.value : a)
                        (v : a)
                        (obj.radius : float)
                        (d : float)]));

    List.iter this.objects ~f:(fun obj ->
        Option.iter obj.tree ~f:(fun tree -> node (Some (obj, this)) tree))
  in
  node None t.root

let empty ?(capacity = 4) compare distance =
  { root = { objects = []; parent = None }; distance; capacity; compare }

let mk_obj ?(parent_dist = Float.nan) ?(radius = 0.) ?(tree = None) v =
  { value = v; parent_dist; radius; tree }

let set_parent p = Option.iter ~f:(fun n -> n.parent <- Some p)

let set_covering_radius r =
  Option.iter r.tree ~f:(fun { objects; _ } ->
      r.radius <-
        Iter.of_list objects
        |> Iter.map (fun obj -> obj.parent_dist +. obj.radius)
        |> Iter.max ~lt:( <. )
        |> Option.value ~default:Float.nan)

let mk_node ?parent objects =
  let node = { objects; parent } in
  List.iter objects ~f:(fun router ->
      set_parent (router, node) router.tree;
      set_covering_radius router);
  node

let is_leaf x = List.for_all x.objects ~f:(fun o -> Option.is_none o.tree)

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
  r.tree <- Some (mk_node l);
  r'.tree <- Some (mk_node l')

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
        ((objects (((value 1) (parent_dist 0.5)) ((value 1.5) (parent_dist 0))))))))
     (r' ((value 2) (tree ((objects (((value 2) (parent_dist 0))))))))
     (o1 ((value 1) (parent_dist 0.5))) (o2 ((value 2) (parent_dist 0)))
     (o3 ((value 1.5) (parent_dist 0)))) |}]

let rec split t split_node v =
  let entries = v :: split_node.objects in
  let r, r' = promote @@ Iter.of_list entries in
  partition t.distance r r' entries;

  match split_node.parent with
  | Some (parent_router, parent_node) ->
      let gparent_dist v =
        match parent_node.parent with
        | Some (gparent_router, _) -> t.distance gparent_router.value v
        | None -> Float.nan
      in

      (* replace this node's router with one of the new routers *)
      parent_node.objects <-
        List.map parent_node.objects ~f:(fun r_old ->
            if t.compare r_old.value parent_router.value = 0 then r else r_old);
      (* the new router is going in the parent, so its new parent is the grandparent *)
      r.parent_dist <- gparent_dist r.value;
      set_parent (r, parent_node) r.tree;
      set_covering_radius r;

      (* insert the other router if possible, otherwise split *)
      if List.length parent_node.objects < t.capacity then (
        parent_node.objects <- r' :: parent_node.objects;
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

let%expect_test "" =
  let tree = empty ~capacity:2 [%compare: float] abs_dist in
  split tree { objects = [ mk_obj 1.; mk_obj 2. ]; parent = None } (mk_obj 1.5);
  print_s [%message (tree : float t)];
  [%expect
    {|
    (tree
     ((root
       ((objects
         (((value 2) (radius 0.5)
           (tree
            ((objects
              (((value 1.5) (parent_dist 0.5)) ((value 2) (parent_dist 0))))
             (parent (((value 2) (radius 0.5) (tree <opaque>)) <opaque>)))))
          ((value 1)
           (tree
            ((objects (((value 1) (parent_dist 0))))
             (parent (((value 1) (tree <opaque>)) <opaque>)))))))))
      (capacity 2) (distance <fun>) (compare <fun>))) |}]

let insert t v =
  let rec insert_node parent_dist node =
    if is_leaf node then
      (* insert into leaf node *)
      if List.length node.objects < t.capacity then
        let obj = { value = v; parent_dist; tree = None; radius = 0. } in
        node.objects <- obj :: node.objects
      else
        (* split full leaf node *)
        split t node { value = v; parent_dist = Float.nan; radius = 0.; tree = None }
    else
      (* find the closest router that would not need a radius increase *)
      let closest_covering =
        Iter.of_list node.objects
        (* use triangle inequality to filter noncovering routers *)
        |> Iter.filter (fun r ->
               Float.is_nan parent_dist || Float.is_nan r.parent_dist
               || Float.abs (parent_dist -. r.parent_dist) <=. r.radius)
        |> Iter.map (fun r -> (t.distance r.value v, r))
        (* check that router can cover the new point *)
        |> Iter.filter (fun (d, r) -> d <=. r.radius)
        |> Iter.min ~lt:(fun (d, _) (d', _) -> d <. d')
      in
      let dist, obj =
        match closest_covering with
        | Some (d, r) -> (d, r)
        | None ->
            (* no router covers the new point, so minimize the increase in covering radius *)
            let dist, obj =
              Iter.of_list node.objects
              |> Iter.map (fun r -> (t.distance r.value v, r))
              |> Iter.min_exn ~lt:(fun (d, r) (d', r') ->
                     d -. r.radius <. d' -. r'.radius)
            in
            obj.radius <- dist;
            (dist, obj)
      in
      insert_node dist (Option.value_exn obj.tree)
  in
  insert_node Float.nan t.root

let range t v r f =
  let rec range_node query_parent_dist x =
    List.iter x.objects ~f:(fun obj ->
        if
          Float.is_nan obj.parent_dist
          || Float.is_nan query_parent_dist
          || Float.abs (query_parent_dist -. obj.parent_dist) <=. r +. obj.radius
        then
          let query_dist = t.distance obj.value v in
          if query_dist <=. r +. obj.radius then
            match obj.tree with
            | None -> f obj.value
            | Some node -> range_node query_dist node)
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
  let tree = empty [%compare: float] abs_dist in
  List.iter points ~f:(fun p -> insert tree p);
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
  let tree = empty [%compare: float] abs_dist in
  List.iteri points ~f:(fun i p ->
      insert tree p;
      assert (Iter.length @@ iter tree = i + 1);
      print_s [%message (p : float) (tree : float t)];
      invariant tree [%sexp_of: float]);
  [%expect
    {|
    ((p 0.1)
     (tree
      ((root ((objects (((value 0.1)))))) (capacity 4) (distance <fun>)
       (compare <fun>))))
    ((p 0.2)
     (tree
      ((root ((objects (((value 0.2)) ((value 0.1)))))) (capacity 4)
       (distance <fun>) (compare <fun>))))
    ((p 0.15)
     (tree
      ((root ((objects (((value 0.15)) ((value 0.2)) ((value 0.1))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.5)
     (tree
      ((root
        ((objects (((value 0.5)) ((value 0.15)) ((value 0.2)) ((value 0.1))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.6)
     (tree
      ((root
        ((objects
          (((value 0.1) (radius 0.1)
            (tree
             ((objects
               (((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.2) (parent_dist 0.1)) ((value 0.1) (parent_dist 0))))
              (parent (((value 0.1) (radius 0.1) (tree <opaque>)) <opaque>)))))
           ((value 0.5) (radius 0.099999999999999978)
            (tree
             ((objects
               (((value 0.6) (parent_dist 0.099999999999999978))
                ((value 0.5) (parent_dist 0))))
              (parent
               (((value 0.5) (radius 0.099999999999999978) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.9)
     (tree
      ((root
        ((objects
          (((value 0.1) (radius 0.1)
            (tree
             ((objects
               (((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.2) (parent_dist 0.1)) ((value 0.1) (parent_dist 0))))
              (parent (((value 0.1) (radius 0.1) (tree <opaque>)) <opaque>)))))
           ((value 0.5) (radius 0.4)
            (tree
             ((objects
               (((value 0.9) (parent_dist 0.4))
                ((value 0.6) (parent_dist 0.099999999999999978))
                ((value 0.5) (parent_dist 0))))
              (parent (((value 0.5) (radius 0.4) (tree <opaque>)) <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.95)
     (tree
      ((root
        ((objects
          (((value 0.1) (radius 0.1)
            (tree
             ((objects
               (((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.2) (parent_dist 0.1)) ((value 0.1) (parent_dist 0))))
              (parent (((value 0.1) (radius 0.1) (tree <opaque>)) <opaque>)))))
           ((value 0.5) (radius 0.44999999999999996)
            (tree
             ((objects
               (((value 0.95) (parent_dist 0.44999999999999996))
                ((value 0.9) (parent_dist 0.4))
                ((value 0.6) (parent_dist 0.099999999999999978))
                ((value 0.5) (parent_dist 0))))
              (parent
               (((value 0.5) (radius 0.44999999999999996) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.8)
     (tree
      ((root
        ((objects
          (((value 0.9) (radius 0.049999999999999933)
            (tree
             ((objects
               (((value 0.95) (parent_dist 0.049999999999999933))
                ((value 0.9) (parent_dist 0))))
              (parent
               (((value 0.9) (radius 0.049999999999999933) (tree <opaque>))
                <opaque>)))))
           ((value 0.1) (radius 0.1)
            (tree
             ((objects
               (((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.2) (parent_dist 0.1)) ((value 0.1) (parent_dist 0))))
              (parent (((value 0.1) (radius 0.1) (tree <opaque>)) <opaque>)))))
           ((value 0.8) (radius 0.30000000000000004)
            (tree
             ((objects
               (((value 0.8) (parent_dist 0))
                ((value 0.6) (parent_dist 0.20000000000000007))
                ((value 0.5) (parent_dist 0.30000000000000004))))
              (parent
               (((value 0.8) (radius 0.30000000000000004) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.65)
     (tree
      ((root
        ((objects
          (((value 0.9) (radius 0.049999999999999933)
            (tree
             ((objects
               (((value 0.95) (parent_dist 0.049999999999999933))
                ((value 0.9) (parent_dist 0))))
              (parent
               (((value 0.9) (radius 0.049999999999999933) (tree <opaque>))
                <opaque>)))))
           ((value 0.1) (radius 0.1)
            (tree
             ((objects
               (((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.2) (parent_dist 0.1)) ((value 0.1) (parent_dist 0))))
              (parent (((value 0.1) (radius 0.1) (tree <opaque>)) <opaque>)))))
           ((value 0.8) (radius 0.30000000000000004)
            (tree
             ((objects
               (((value 0.65) (parent_dist 0.15000000000000002))
                ((value 0.8) (parent_dist 0))
                ((value 0.6) (parent_dist 0.20000000000000007))
                ((value 0.5) (parent_dist 0.30000000000000004))))
              (parent
               (((value 0.8) (radius 0.30000000000000004) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.01)
     (tree
      ((root
        ((objects
          (((value 0.9) (radius 0.049999999999999933)
            (tree
             ((objects
               (((value 0.95) (parent_dist 0.049999999999999933))
                ((value 0.9) (parent_dist 0))))
              (parent
               (((value 0.9) (radius 0.049999999999999933) (tree <opaque>))
                <opaque>)))))
           ((value 0.1) (radius 0.1)
            (tree
             ((objects
               (((value 0.01) (parent_dist 0.090000000000000011))
                ((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.2) (parent_dist 0.1)) ((value 0.1) (parent_dist 0))))
              (parent (((value 0.1) (radius 0.1) (tree <opaque>)) <opaque>)))))
           ((value 0.8) (radius 0.30000000000000004)
            (tree
             ((objects
               (((value 0.65) (parent_dist 0.15000000000000002))
                ((value 0.8) (parent_dist 0))
                ((value 0.6) (parent_dist 0.20000000000000007))
                ((value 0.5) (parent_dist 0.30000000000000004))))
              (parent
               (((value 0.8) (radius 0.30000000000000004) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.21)
     (tree
      ((root
        ((objects
          (((value 0.1) (radius 0.090000000000000011)
            (tree
             ((objects
               (((value 0.01) (parent_dist 0.090000000000000011))
                ((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.1) (parent_dist 0))))
              (parent
               (((value 0.1) (radius 0.090000000000000011) (tree <opaque>))
                <opaque>)))))
           ((value 0.9) (radius 0.049999999999999933)
            (tree
             ((objects
               (((value 0.95) (parent_dist 0.049999999999999933))
                ((value 0.9) (parent_dist 0))))
              (parent
               (((value 0.9) (radius 0.049999999999999933) (tree <opaque>))
                <opaque>)))))
           ((value 0.21) (radius 0.0099999999999999811)
            (tree
             ((objects
               (((value 0.21) (parent_dist 0))
                ((value 0.2) (parent_dist 0.0099999999999999811))))
              (parent
               (((value 0.21) (radius 0.0099999999999999811) (tree <opaque>))
                <opaque>)))))
           ((value 0.8) (radius 0.30000000000000004)
            (tree
             ((objects
               (((value 0.65) (parent_dist 0.15000000000000002))
                ((value 0.8) (parent_dist 0))
                ((value 0.6) (parent_dist 0.20000000000000007))
                ((value 0.5) (parent_dist 0.30000000000000004))))
              (parent
               (((value 0.8) (radius 0.30000000000000004) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.23)
     (tree
      ((root
        ((objects
          (((value 0.1) (radius 0.090000000000000011)
            (tree
             ((objects
               (((value 0.01) (parent_dist 0.090000000000000011))
                ((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.1) (parent_dist 0))))
              (parent
               (((value 0.1) (radius 0.090000000000000011) (tree <opaque>))
                <opaque>)))))
           ((value 0.9) (radius 0.049999999999999933)
            (tree
             ((objects
               (((value 0.95) (parent_dist 0.049999999999999933))
                ((value 0.9) (parent_dist 0))))
              (parent
               (((value 0.9) (radius 0.049999999999999933) (tree <opaque>))
                <opaque>)))))
           ((value 0.21) (radius 0.020000000000000018)
            (tree
             ((objects
               (((value 0.23) (parent_dist 0.020000000000000018))
                ((value 0.21) (parent_dist 0))
                ((value 0.2) (parent_dist 0.0099999999999999811))))
              (parent
               (((value 0.21) (radius 0.020000000000000018) (tree <opaque>))
                <opaque>)))))
           ((value 0.8) (radius 0.30000000000000004)
            (tree
             ((objects
               (((value 0.65) (parent_dist 0.15000000000000002))
                ((value 0.8) (parent_dist 0))
                ((value 0.6) (parent_dist 0.20000000000000007))
                ((value 0.5) (parent_dist 0.30000000000000004))))
              (parent
               (((value 0.8) (radius 0.30000000000000004) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.88)
     (tree
      ((root
        ((objects
          (((value 0.1) (radius 0.090000000000000011)
            (tree
             ((objects
               (((value 0.01) (parent_dist 0.090000000000000011))
                ((value 0.15) (parent_dist 0.049999999999999989))
                ((value 0.1) (parent_dist 0))))
              (parent
               (((value 0.1) (radius 0.090000000000000011) (tree <opaque>))
                <opaque>)))))
           ((value 0.9) (radius 0.049999999999999933)
            (tree
             ((objects
               (((value 0.88) (parent_dist 0.020000000000000018))
                ((value 0.95) (parent_dist 0.049999999999999933))
                ((value 0.9) (parent_dist 0))))
              (parent
               (((value 0.9) (radius 0.049999999999999933) (tree <opaque>))
                <opaque>)))))
           ((value 0.21) (radius 0.020000000000000018)
            (tree
             ((objects
               (((value 0.23) (parent_dist 0.020000000000000018))
                ((value 0.21) (parent_dist 0))
                ((value 0.2) (parent_dist 0.0099999999999999811))))
              (parent
               (((value 0.21) (radius 0.020000000000000018) (tree <opaque>))
                <opaque>)))))
           ((value 0.8) (radius 0.30000000000000004)
            (tree
             ((objects
               (((value 0.65) (parent_dist 0.15000000000000002))
                ((value 0.8) (parent_dist 0))
                ((value 0.6) (parent_dist 0.20000000000000007))
                ((value 0.5) (parent_dist 0.30000000000000004))))
              (parent
               (((value 0.8) (radius 0.30000000000000004) (tree <opaque>))
                <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.74)
     (tree
      ((root
        ((objects
          (((value 0.21) (radius 0.2)
            (tree
             ((objects
               (((value 0.1) (parent_dist 0.10999999999999999)
                 (radius 0.090000000000000011)
                 (tree
                  ((objects
                    (((value 0.01) (parent_dist 0.090000000000000011))
                     ((value 0.15) (parent_dist 0.049999999999999989))
                     ((value 0.1) (parent_dist 0))))
                   (parent
                    (((value 0.1) (parent_dist 0.10999999999999999)
                      (radius 0.090000000000000011) (tree <opaque>))
                     <opaque>)))))
                ((value 0.21) (parent_dist 0) (radius 0.020000000000000018)
                 (tree
                  ((objects
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
             ((objects
               (((value 0.65) (parent_dist 0.050000000000000044)
                 (radius 0.15000000000000002)
                 (tree
                  ((objects
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
                  ((objects
                    (((value 0.88) (parent_dist 0.020000000000000018))
                     ((value 0.95) (parent_dist 0.049999999999999933))
                     ((value 0.9) (parent_dist 0))))
                   (parent
                    (((value 0.9) (parent_dist 0.30000000000000004)
                      (radius 0.049999999999999933) (tree <opaque>))
                     <opaque>)))))
                ((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                 (tree
                  ((objects
                    (((value 0.6) (parent_dist 0))
                     ((value 0.5) (parent_dist 0.099999999999999978))))
                   (parent
                    (((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.6) (radius 0.35) (tree <opaque>)) <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.62)
     (tree
      ((root
        ((objects
          (((value 0.21) (radius 0.2)
            (tree
             ((objects
               (((value 0.1) (parent_dist 0.10999999999999999)
                 (radius 0.090000000000000011)
                 (tree
                  ((objects
                    (((value 0.01) (parent_dist 0.090000000000000011))
                     ((value 0.15) (parent_dist 0.049999999999999989))
                     ((value 0.1) (parent_dist 0))))
                   (parent
                    (((value 0.1) (parent_dist 0.10999999999999999)
                      (radius 0.090000000000000011) (tree <opaque>))
                     <opaque>)))))
                ((value 0.21) (parent_dist 0) (radius 0.020000000000000018)
                 (tree
                  ((objects
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
             ((objects
               (((value 0.65) (parent_dist 0.050000000000000044)
                 (radius 0.15000000000000002)
                 (tree
                  ((objects
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
                  ((objects
                    (((value 0.88) (parent_dist 0.020000000000000018))
                     ((value 0.95) (parent_dist 0.049999999999999933))
                     ((value 0.9) (parent_dist 0))))
                   (parent
                    (((value 0.9) (parent_dist 0.30000000000000004)
                      (radius 0.049999999999999933) (tree <opaque>))
                     <opaque>)))))
                ((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                 (tree
                  ((objects
                    (((value 0.62) (parent_dist 0.020000000000000018))
                     ((value 0.6) (parent_dist 0))
                     ((value 0.5) (parent_dist 0.099999999999999978))))
                   (parent
                    (((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.6) (radius 0.35) (tree <opaque>)) <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.55)
     (tree
      ((root
        ((objects
          (((value 0.21) (radius 0.2)
            (tree
             ((objects
               (((value 0.1) (parent_dist 0.10999999999999999)
                 (radius 0.090000000000000011)
                 (tree
                  ((objects
                    (((value 0.01) (parent_dist 0.090000000000000011))
                     ((value 0.15) (parent_dist 0.049999999999999989))
                     ((value 0.1) (parent_dist 0))))
                   (parent
                    (((value 0.1) (parent_dist 0.10999999999999999)
                      (radius 0.090000000000000011) (tree <opaque>))
                     <opaque>)))))
                ((value 0.21) (parent_dist 0) (radius 0.020000000000000018)
                 (tree
                  ((objects
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
             ((objects
               (((value 0.65) (parent_dist 0.050000000000000044)
                 (radius 0.15000000000000002)
                 (tree
                  ((objects
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
                  ((objects
                    (((value 0.88) (parent_dist 0.020000000000000018))
                     ((value 0.95) (parent_dist 0.049999999999999933))
                     ((value 0.9) (parent_dist 0))))
                   (parent
                    (((value 0.9) (parent_dist 0.30000000000000004)
                      (radius 0.049999999999999933) (tree <opaque>))
                     <opaque>)))))
                ((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                 (tree
                  ((objects
                    (((value 0.55) (parent_dist 0.049999999999999933))
                     ((value 0.62) (parent_dist 0.020000000000000018))
                     ((value 0.6) (parent_dist 0))
                     ((value 0.5) (parent_dist 0.099999999999999978))))
                   (parent
                    (((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.6) (radius 0.35) (tree <opaque>)) <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.32)
     (tree
      ((root
        ((objects
          (((value 0.21) (radius 0.2)
            (tree
             ((objects
               (((value 0.1) (parent_dist 0.10999999999999999)
                 (radius 0.090000000000000011)
                 (tree
                  ((objects
                    (((value 0.01) (parent_dist 0.090000000000000011))
                     ((value 0.15) (parent_dist 0.049999999999999989))
                     ((value 0.1) (parent_dist 0))))
                   (parent
                    (((value 0.1) (parent_dist 0.10999999999999999)
                      (radius 0.090000000000000011) (tree <opaque>))
                     <opaque>)))))
                ((value 0.21) (parent_dist 0) (radius 0.11000000000000001)
                 (tree
                  ((objects
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
             ((objects
               (((value 0.65) (parent_dist 0.050000000000000044)
                 (radius 0.15000000000000002)
                 (tree
                  ((objects
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
                  ((objects
                    (((value 0.88) (parent_dist 0.020000000000000018))
                     ((value 0.95) (parent_dist 0.049999999999999933))
                     ((value 0.9) (parent_dist 0))))
                   (parent
                    (((value 0.9) (parent_dist 0.30000000000000004)
                      (radius 0.049999999999999933) (tree <opaque>))
                     <opaque>)))))
                ((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                 (tree
                  ((objects
                    (((value 0.55) (parent_dist 0.049999999999999933))
                     ((value 0.62) (parent_dist 0.020000000000000018))
                     ((value 0.6) (parent_dist 0))
                     ((value 0.5) (parent_dist 0.099999999999999978))))
                   (parent
                    (((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.6) (radius 0.35) (tree <opaque>)) <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.12)
     (tree
      ((root
        ((objects
          (((value 0.21) (radius 0.2)
            (tree
             ((objects
               (((value 0.1) (parent_dist 0.10999999999999999)
                 (radius 0.090000000000000011)
                 (tree
                  ((objects
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
                  ((objects
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
             ((objects
               (((value 0.65) (parent_dist 0.050000000000000044)
                 (radius 0.15000000000000002)
                 (tree
                  ((objects
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
                  ((objects
                    (((value 0.88) (parent_dist 0.020000000000000018))
                     ((value 0.95) (parent_dist 0.049999999999999933))
                     ((value 0.9) (parent_dist 0))))
                   (parent
                    (((value 0.9) (parent_dist 0.30000000000000004)
                      (radius 0.049999999999999933) (tree <opaque>))
                     <opaque>)))))
                ((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                 (tree
                  ((objects
                    (((value 0.55) (parent_dist 0.049999999999999933))
                     ((value 0.62) (parent_dist 0.020000000000000018))
                     ((value 0.6) (parent_dist 0))
                     ((value 0.5) (parent_dist 0.099999999999999978))))
                   (parent
                    (((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.6) (radius 0.35) (tree <opaque>)) <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>))))
    ((p 0.14)
     (tree
      ((root
        ((objects
          (((value 0.21) (radius 0.2)
            (tree
             ((objects
               (((value 0.15) (parent_dist 0.06)
                 (tree
                  ((objects (((value 0.15) (parent_dist 0))))
                   (parent
                    (((value 0.15) (parent_dist 0.06) (tree <opaque>)) <opaque>)))))
                ((value 0.14) (parent_dist 0.069999999999999979) (radius 0.13)
                 (tree
                  ((objects
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
                  ((objects
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
             ((objects
               (((value 0.65) (parent_dist 0.050000000000000044)
                 (radius 0.15000000000000002)
                 (tree
                  ((objects
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
                  ((objects
                    (((value 0.88) (parent_dist 0.020000000000000018))
                     ((value 0.95) (parent_dist 0.049999999999999933))
                     ((value 0.9) (parent_dist 0))))
                   (parent
                    (((value 0.9) (parent_dist 0.30000000000000004)
                      (radius 0.049999999999999933) (tree <opaque>))
                     <opaque>)))))
                ((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                 (tree
                  ((objects
                    (((value 0.55) (parent_dist 0.049999999999999933))
                     ((value 0.62) (parent_dist 0.020000000000000018))
                     ((value 0.6) (parent_dist 0))
                     ((value 0.5) (parent_dist 0.099999999999999978))))
                   (parent
                    (((value 0.6) (parent_dist 0) (radius 0.099999999999999978)
                      (tree <opaque>))
                     <opaque>)))))))
              (parent (((value 0.6) (radius 0.35) (tree <opaque>)) <opaque>)))))))))
       (capacity 4) (distance <fun>) (compare <fun>)))) |}]

let%test_unit "" =
  let tree = empty [%compare: float] abs_dist in
  for _ = 0 to 1000 do
    insert tree (Random.float 1.);
    invariant tree [%sexp_of: float]
  done
