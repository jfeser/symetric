open Std

type 'a routing = { value : 'a; radius : float; tree : 'a node }

and 'a node =
  | Nil
  | Leaf of { mutable objects : 'a list; mutable parent : 'a node option }
  | Internal of { mutable routers : 'a routing list; mutable parent : 'a node option }
[@@deriving sexp]

type 'a t = {
  mutable root : 'a node;
  capacity : int;
  distance : 'a -> 'a -> float;
  default : 'a;
}
[@@deriving sexp]

let empty ?(capacity = 4) ~default distance =
  { root = Leaf { objects = []; parent = None }; distance; capacity; default }

let parent = function Nil -> None | Leaf x -> x.parent | Internal x -> x.parent

let set_parent p = function
  | Nil -> ()
  | Leaf x -> x.parent <- Some p
  | Internal x -> x.parent <- Some p

let entries = function
  | Nil -> Iter.empty
  | Leaf x ->
      Iter.of_list x.objects |> Iter.map (fun v -> { value = v; radius = 0.; tree = Nil })
  | Internal x -> Iter.of_list x.routers |> Iter.map (fun r -> { r with tree = Nil })

let promote entries =
  let ret = Iter.sample 2 entries in
  (ret.(0), ret.(1))

let partition t r r' = function
  | Leaf x ->
      let l, l' =
        List.partition_tf x.objects ~f:(fun v ->
            t.distance v r.value <=. t.distance v r'.value)
      in
      ( { r with tree = Leaf { objects = l; parent = None } },
        { r' with tree = Leaf { objects = l'; parent = None } } )
  | Internal x ->
      let l, l' =
        List.partition_tf x.routers ~f:(fun v ->
            t.distance v.value r.value <=. t.distance v.value r'.value)
      in
      ( { r with tree = Internal { routers = l; parent = None } },
        { r' with tree = Internal { routers = l'; parent = None } } )
  | Nil -> assert false

let rec split t node v =
  let r, r' = promote @@ Iter.cons v @@ entries node in
  let r, r' = partition t r r' node in
  match parent node with
  | Some (Internal x as parent_node) ->
      (* replace this node's router with one of the new routers *)
      x.routers <-
        List.map x.routers ~f:(fun r_old ->
            if phys_equal r_old.tree node then r else r_old);
      set_parent r.tree parent_node;

      (* insert the other router if possible, otherwise split *)
      if List.length x.routers < t.capacity then (
        x.routers <- r' :: x.routers;
        set_parent r.tree parent_node)
      else split t parent_node r'
  | None ->
      (* this node was the root, so create a new root *)
      let root = Internal { routers = [ r; r' ]; parent = None } in
      set_parent r.tree root;
      set_parent r'.tree root;
      t.root <- root
  | Some (Leaf _ | Nil) -> assert false

let rec insert t v = function
  | Leaf ({ objects } as l) when List.length objects < t.capacity ->
      l.objects <- v :: objects
  | Leaf _ as node -> split t node { value = v; radius = 0.; tree = Nil }
  | Internal { routers } ->
      let distances = List.map routers ~f:(fun r -> (t.distance r.value v, r)) in
      let closest_covering =
        Iter.of_list distances
        |> Iter.filter (fun (d, r) -> d <=. r.radius)
        |> Iter.min ~lt:(fun (d, _) (d', _) -> d <. d')
      in
      let insert_here =
        match closest_covering with
        | Some (_, i) -> i
        | None ->
            let _, i =
              Iter.of_list distances
              |> Iter.min_exn ~lt:(fun (d, r) (d', r') ->
                     d -. r.radius <. d' -. r'.radius)
            in
            i
      in
      insert t v insert_here.tree
  | Nil -> assert false
