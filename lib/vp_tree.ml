(* Vantage-point tree implementation
   Cf. "Data structures and algorithms for nearest neighbor search
   in general metric spaces" by Peter N. Yianilos for details.
   http://citeseerx.ist.psu.edu/viewdoc/\
   download?doi=10.1.1.41.4193&rep=rep1&type=pdf *)

(* Functorial interface *)
open Caml
module Option = Std.Option

module Array = struct
  include Array

  (* smaller array, without elt at index 'i' *)
  let remove i a =
    let n = length a in
    assert (i >= 0 && i < n);
    let res = make (n - 1) (unsafe_get a 0) in
    let j = ref 0 in
    for i' = 0 to n - 1 do
      if i' <> i then (
        unsafe_set res !j (unsafe_get a i');
        incr j)
    done;
    res

  (* <=> List.partition *)
  let partition p a =
    let ok, ko =
      fold_right (fun x (ok_acc, ko_acc) -> if p x then (x :: ok_acc, ko_acc) else (ok_acc, x :: ko_acc)) a ([], [])
    in
    (of_list ok, of_list ko)

  (* <=> List.split *)
  let split a =
    let n = length a in
    if n = 0 then ([||], [||])
    else
      let l, r = unsafe_get a 0 in
      let left = make n l in
      let right = make n r in
      for i = 1 to n - 1 do
        let l, r = unsafe_get a i in
        unsafe_set left i l;
        unsafe_set right i r
      done;
      (left, right)

  (* <=> BatArray.min_max with default value in case of empty array *)
  let min_max_def a def =
    let n = length a in
    if n = 0 then def
    else
      let mini = ref (unsafe_get a 0) in
      let maxi = ref (unsafe_get a 0) in
      for i = 1 to n - 1 do
        let x = unsafe_get a i in
        if x < !mini then mini := x;
        if x > !maxi then maxi := x
      done;
      (!mini, !maxi)

  (* get one bootstrap sample of 'size' using sampling with replacement *)
  let bootstrap_sample rng size a =
    let n = length a in
    assert (n > 0);
    assert (size < n);
    let res = make size (unsafe_get a 0) in
    for i = 0 to size - 1 do
      let rand = Base.Random.State.int rng n in
      unsafe_set res i (unsafe_get a rand)
    done;
    res
end

module type Point = sig
  type t [@@deriving sexp]

  (* dist _must_ be a metric *)
  val dist : t -> t -> float
end

module Make (P : Point) = struct
  type quality =
    | Optimal (* if you have thousands of points *)
    | Good of int (* if you have tens to hundreds
                     of thousands of points *)
    | Random
  (* if you have millions of points *)

  type node = {
    vp : P.t;
    lb_low : float;
    lb_high : float;
    middle : float;
    rb_low : float;
    rb_high : float;
    left : t;
    right : t;
    length : int;
  }

  and t = Empty | Node of node [@@deriving sexp]

  let length = function Empty -> 0 | Node v -> v.length

  let new_node vp lb_low lb_high middle rb_low rb_high left right =
    Node { vp; lb_low; lb_high; middle; rb_low; rb_high; left; right; length = length left + length right + 1 }

  type open_itv = { lbound : float; rbound : float }

  let new_open_itv lbound rbound =
    assert (lbound <= rbound);
    { lbound; rbound }

  let in_open_itv x { lbound; rbound } = x > lbound && x < rbound

  let itv_dont_overlap left right =
    let a = left.lbound in
    let b = left.rbound in
    let c = right.lbound in
    let d = right.rbound in
    (* [a..b] [c..d] OR [c..d] [a..b] *)
    b < c || d < a

  let itv_overlap left right = not (itv_dont_overlap left right)

  let square (x : float) : float = x *. x

  let float_compare (x : float) (y : float) : int = if x < y then -1 else if x > y then 1 else 0
  (* x = y *)

  let median (xs : float array) : float =
    Array.sort float_compare xs;
    let n = Array.length xs in
    if n mod 2 = 1 then xs.(n / 2) else 0.5 *. (xs.(n / 2) +. xs.((n / 2) - 1))

  let variance (mu : float) (xs : float array) : float = Array.fold_left (fun acc x -> acc +. square (x -. mu)) 0.0 xs

  (* compute distance of point at index 'q_i' to all other points *)
  let distances (q_i : int) (points : P.t array) : float array =
    let n = Array.length points in
    assert (n > 1);
    let res = Array.make (n - 1) 0.0 in
    let j = ref 0 in
    let q = points.(q_i) in
    for i = 0 to n - 1 do
      if i <> q_i then (
        res.(!j) <- P.dist q points.(i);
        incr j)
    done;
    res

  (* this is optimal (slowest tree construction; O(n^2));
     but fastest query time *)
  let select_best_vp (points : P.t array) =
    let n = Array.length points in
    if n = 0 then assert false
    else if n = 1 then (points.(0), 0.0, [||])
    else
      let curr_vp = ref 0 in
      let curr_mu = ref 0.0 in
      let curr_spread = ref 0.0 in
      for i = 0 to n - 1 do
        (* could be faster using a distance cache? *)
        let dists = distances !curr_vp points in
        let mu = median dists in
        let spread = variance mu dists in
        if spread > !curr_spread then (
          curr_vp := i;
          curr_mu := mu;
          curr_spread := spread)
      done;
      (points.(!curr_vp), !curr_mu, Array.remove !curr_vp points)

  (* to replace select_best_vp when working with too many points *)
  let select_good_vp rng (sample_size : int) (points : P.t array) =
    let n = Array.length points in
    if sample_size * sample_size >= n then select_best_vp points
    else
      let candidates = Array.bootstrap_sample rng sample_size points in
      let curr_vp = ref 0 in
      let curr_mu = ref 0.0 in
      let curr_spread = ref 0.0 in
      Array.iteri
        (fun i p_i ->
          let sample = Array.bootstrap_sample rng sample_size points in
          let dists = Array.map (P.dist p_i) sample in
          let mu = median dists in
          let spread = variance mu dists in
          if spread > !curr_spread then (
            curr_vp := i;
            curr_mu := mu;
            curr_spread := spread))
        candidates;
      (* we need the true mu to balance the tree;
         not the one gotten from the sample! *)
      let dists = distances !curr_vp points in
      let mu = median dists in
      (points.(!curr_vp), mu, Array.remove !curr_vp points)

  (* to replace select_good_vp when working with way too many points,
     or if you really need the fastest possible tree construction *)
  let select_rand_vp rng (points : P.t array) =
    let n = Array.length points in
    assert (n > 0);
    let vp = Base.Random.State.int rng n in
    let dists = distances vp points in
    let mu = median dists in
    (points.(vp), mu, Array.remove vp points)

  exception Empty_list

  let rec create' select_vp points =
    let n = Array.length points in
    if n = 0 then Empty
    else if n = 1 then new_node points.(0) 0. 0. 0. 0. 0. Empty Empty
    else
      let vp, mu, others = select_vp points in
      let dists = Array.map (fun p -> (P.dist vp p, p)) others in
      let lefties, righties = Array.partition (fun (d, _) -> d < mu) dists in
      let ldists, lpoints = Array.split lefties in
      let rdists, rpoints = Array.split righties in
      let lb_low, lb_high = Array.min_max_def ldists (0., 0.) in
      let rb_low, rb_high = Array.min_max_def rdists (0., 0.) in
      let middle = (lb_high +. rb_low) *. 0.5 in
      new_node vp lb_low lb_high middle rb_low rb_high (create' select_vp lpoints) (create' select_vp rpoints)

  let create ?state quality points =
    let state = Option.value_lazy ~default:(lazy (Base.Random.State.make_self_init ())) state in
    let select_vp =
      match quality with
      | Optimal -> select_best_vp
      | Good ssize -> select_good_vp state ssize
      | Random -> select_rand_vp state
    in
    create' select_vp (Array.of_list points)

  let rec find_nearest acc query tree =
    match tree with
    | Empty -> acc
    | Node { vp; lb_low; lb_high; middle; rb_low; rb_high; left; right } -> (
        let x = P.dist vp query in
        if x = 0.0 then Some (x, vp) (* can't get nearer than that *)
        else
          let tau, acc' =
            match acc with
            | None -> (x, Some (x, vp))
            | Some (tau, best) -> if x < tau then (x, Some (x, vp)) else (tau, Some (tau, best))
          in
          let il = new_open_itv (lb_low -. tau) (lb_high +. tau) in
          let ir = new_open_itv (rb_low -. tau) (rb_high +. tau) in
          let in_il = in_open_itv x il in
          let in_ir = in_open_itv x ir in
          if x < middle then
            match (in_il, in_ir) with
            | false, false -> acc'
            | true, false -> find_nearest acc' query left
            | false, true -> find_nearest acc' query right
            | true, true -> (
                match find_nearest acc' query left with
                | None -> find_nearest acc' query right
                | Some (tau, best) -> (
                    match find_nearest acc' query right with
                    | None -> Some (tau, best)
                    | Some (tau', best') -> if tau' < tau then Some (tau', best') else Some (tau, best)))
          else
            (* x >= middle *)
            match (in_ir, in_il) with
            | false, false -> acc'
            | true, false -> find_nearest acc' query right
            | false, true -> find_nearest acc' query left
            | true, true -> (
                match find_nearest acc' query right with
                | None -> find_nearest acc' query left
                | Some (tau, best) -> (
                    match find_nearest acc' query left with
                    | None -> Some (tau, best)
                    | Some (tau', best') -> if tau' < tau then Some (tau', best') else Some (tau, best))))

  let nearest_neighbor query tree = match find_nearest None query tree with Some x -> x | None -> raise Not_found

  let rec to_list_loop acc = function
    | Empty -> acc
    | Node n ->
        let acc' = to_list_loop acc n.right in
        to_list_loop (n.vp :: acc') n.left

  let to_list tree = to_list_loop [] tree

  let neighbors query tol tree =
    let rec loop acc = function
      | Empty -> acc
      | Node { vp; lb_low; lb_high; rb_low; rb_high; left; right } ->
          (* should we include vp? *)
          let d = P.dist vp query in
          let acc' = if d <= tol then vp :: acc else acc in
          let lbound = max 0.0 (d -. tol) in
          let rbound = d +. tol in
          let itv = new_open_itv lbound rbound in
          (* should we inspect the left? *)
          let lmatches =
            let itv_left = new_open_itv lb_low lb_high in
            if itv_overlap itv itv_left then
              (* further calls to P.dist needed? *)
              if d +. lb_high <= tol then (* all descendants are included *)
                to_list_loop acc' left else loop acc' left
            else acc'
          in
          (* should we inspect the right? *)
          let itv_right = new_open_itv rb_low rb_high in
          if itv_overlap itv itv_right then
            (* further calls to P.dist needed? *)
            if d +. rb_high <= tol then to_list_loop lmatches right else loop lmatches right
          else lmatches
    in
    loop [] tree

  let is_empty = function Empty -> true | Node _ -> false

  let root = function Empty -> raise Not_found | Node { vp } -> vp

  (* test if the tree invariant holds.
     If it doesn't, then we are in trouble... *)
  let rec check = function
    | Empty -> true
    | Node { vp; lb_low; lb_high; middle; rb_low; rb_high; left; right } ->
        let bounds_OK =
          0.0 <= lb_low && lb_low <= lb_high
          && (lb_high < middle || 0.0 = middle)
          && middle <= rb_low && rb_low <= rb_high
        in
        bounds_OK
        && List.for_all (fun p -> P.dist vp p < middle) (to_list left)
        && List.for_all (fun p -> P.dist vp p >= middle) (to_list right)
        && check left && check right

  exception Found of P.t

  let find query tree =
    let rec loop = function
      | Empty -> ()
      | Node { vp; middle; left; right } ->
          let d = P.dist vp query in
          if d = 0.0 then raise (Found vp) else if d < middle then loop left else loop right
    in
    try
      loop tree;
      raise Not_found
    with Found p -> p

  let mem query tree =
    try
      ignore (find query tree : P.t);
      true
    with Not_found -> false
end
