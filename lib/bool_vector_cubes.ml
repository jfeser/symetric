(* open Params
 * 
 * module T = struct
 *   type cube = {
 *     xmin : float;
 *     xmax : float;
 *     ymin : float;
 *     ymax : float;
 *     zmin : float;
 *     zmax : float;
 *   }
 *   [@@deriving compare, hash, sexp]
 * 
 *   type t = cube list option [@@deriving compare, hash, sexp]
 * end
 * 
 * include T
 * include Comparator.Make (T)
 * 
 * type concrete = bool array
 * 
 * let top = Some []
 * 
 * let bot = None
 * 
 * let is_bottom = Option.is_none
 * 
 * let log_size params = function
 *   | Bottom -> Float.(-infinity)
 *   | Map m -> Float.of_int (Array.length params.bench.output - Map.length m)
 * 
 * let pp : t Fmt.t =
 *   Fmt.using (function
 *     | Map m -> Map.to_alist m |> List.map ~f:(fun (k, v) -> (Bool.to_int v, k))
 *     | Bottom -> [])
 *   @@ Fmt.list ~sep:(Fmt.any " ")
 *   @@ Fmt.pair ~sep:Fmt.nop Fmt.int (Fmt.fmt "_%d")
 * 
 * let graphviz_pp params fmt m =
 *   if not params.hide_values then
 *     let pp =
 *       Fmt.using (function
 *         | Bottom -> []
 *         | Map m ->
 *             Map.to_alist m |> List.map ~f:(fun (k, v) -> (Bool.to_int v, k)))
 *       @@ Fmt.list ~sep:(Fmt.any " ")
 *       @@ Fmt.pair ~sep:Fmt.nop Fmt.int (Fmt.fmt "<sub>%d</sub>")
 *     in
 *     Fmt.pf fmt "%a" pp m
 * 
 * let cube_valid c =
 *   if Float.(c.xmin < c.xmax && c.ymin < c.ymax && c.zmin < c.zmax) then Some c
 *   else None
 * 
 * let cube_inter c c' =
 *   cube_valid
 *     {
 *       xmin = Float.max c.xmin c'.xmin;
 *       xmax = Float.min c.xmax c'.xmax;
 *       ymin = Float.max c.ymin c'.ymin;
 *       ymax = Float.min c.ymax c'.ymax;
 *       zmin = Float.max c.zmin c'.zmin;
 *       zmax = Float.min c.zmax c'.zmax;
 *     }
 * 
 * let cube_is_subset c ~of_:c' =
 *   Float.(
 *     c.xmin >= c'.xmin && c.xmax <= c'.xmax && c.ymin >= c'.ymin
 *     && c.ymax <= c'.ymax && c.zmin >= c'.zmin && c.zmax <= c'.zmax)
 * 
 * let meet =
 *   Option.map2 ~f:(fun cs cs' ->
 *       List.cartesian_product cs cs'
 *       |> List.filter_map ~f:(fun (c, c') -> cube_inter c c'))
 * 
 * let log_overlap params s s' =
 *   Float.(
 *     log_size params (meet s s')
 *     - Float.min (log_size params s) (log_size params s'))
 * 
 * let is_subset s ~of_:s' =
 *   List.for_all s ~f:(fun c ->
 *       List.exists s' ~f:(fun c' -> cube_is_subset c ~of_:c'))
 * 
 * let lift s =
 *   Map
 *     ( Array.mapi s ~f:(fun i x -> (i, x))
 *     |> Array.to_list
 *     |> Map.of_alist_exn (module Int) )
 * 
 * let to_symb ?(prefix = sprintf "b%d") n =
 *   let open Symb0.Bool_vector in
 *   function
 *   | Map m ->
 *       Smt.(
 *         List.init n ~f:(fun b ->
 *             match Map.find m b with
 *             | Some v -> return @@ Fixed v
 *             | None -> fresh_decl ~prefix:(prefix b) () >>| fun v -> Free v)
 *         |> all)
 *   | Bottom -> failwith "bottom"
 * 
 * let union x x' =
 *   match (x, x') with
 *   | Bottom, _ | _, Bottom -> Bottom
 *   | Map x, Map x' ->
 *       Map
 *         (Map.merge x x' ~f:(fun ~key:_ -> function
 *            | `Both (x, x') -> Some (x || x')
 *            | `Left true | `Right true -> Some true
 *            | `Left false | `Right false -> None))
 * 
 * let inter x x' =
 *   match (x, x') with
 *   | Bottom, _ | _, Bottom -> Bottom
 *   | Map x, Map x' ->
 *       Map
 *         (Map.merge x x' ~f:(fun ~key:_ -> function
 *            | `Both (x, x') -> Some (x && x')
 *            | `Left false | `Right false -> Some false
 *            | `Left true | `Right true -> None))
 * 
 * let sub x x' =
 *   match (x, x') with
 *   | Bottom, _ | _, Bottom -> Bottom
 *   | Map x, Map x' ->
 *       Map
 *         (Map.merge x x' ~f:(fun ~key:_ -> function
 *            | `Both (x, x') -> Some (x && not x')
 *            | `Left false | `Right true -> Some false
 *            | `Left true | `Right false -> None))
 * 
 * let mem v i = match v with Bottom -> false | Map v -> Map.mem v i
 * 
 * let set m k v =
 *   match m with Map m -> Map (Map.set m ~key:k ~data:v) | Bottom -> Bottom
 * 
 * let add m k v =
 *   match m with
 *   | Map m -> (
 *       match Map.find m k with
 *       | Some v' -> if Bool.(v = v') then Map m else Bottom
 *       | None -> set (Map m) k v )
 *   | Bottom -> Bottom
 * 
 * let contains a c =
 *   match a with
 *   | Map a -> Map.for_alli a ~f:(fun ~key:i ~data:v -> Bool.(c.(i) = v))
 *   | Bottom -> false
 * 
 * let width a = match a with Map m -> Map.length m | Bottom -> 0
 * 
 * let of_list_exn l = Map (Map.of_alist_exn (module Int) l) *)
