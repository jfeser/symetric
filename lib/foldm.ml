module FoldM0
    (M : Monad.S) (F : sig
      type t
      type elem

      val fold : t -> init:'b -> f:('b -> elem -> 'b) -> 'b
    end) =
struct
  let fold ~f ~init c =
    let open M.Let_syntax in
    F.fold
      ~f:(fun acc x ->
        let%bind acc = acc in
        f acc x)
      ~init:(return init) c

  let iter ~f c = fold ~f:(fun () x -> f x) ~init:() c

  let map ~f c =
    let open M.Let_syntax in
    let%map acc =
      fold ~init:[]
        ~f:(fun acc x ->
          let%map x' = f x in
          x' :: acc)
        c
    in
    List.rev acc
end

module FoldM
    (M : Monad.S) (F : sig
      type 'a t

      val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
    end) =
struct
  let fold (type a) ~f ~init (c : a F.t) =
    let module F0 = struct
      type t = a F.t
      type elem = a

      let fold = F.fold
    end in
    let module Fold = FoldM0 (M) (F0) in
    Fold.fold ~f ~init c

  let iter ~f c = fold ~f:(fun () x -> f x) ~init:() c
end

module FoldM2
    (M : Monad.S) (F : sig
      type ('a, 'b) t

      val fold : ('b, 'c) t -> init:'a -> f:('a -> 'b -> 'a) -> 'a
    end) =
struct
  let fold (type a b) ~f ~init (c : (a, b) F.t) =
    let module F0 = struct
      type t = (a, b) F.t
      type elem = a

      let fold = F.fold
    end in
    let module Fold = FoldM0 (M) (F0) in
    Fold.fold ~f ~init c

  let iter ~f c = fold ~f:(fun () x -> f x) ~init:() c
end
