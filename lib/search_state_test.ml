(* module Lang = struct *)
(*   module Type = struct *)
(*     type t = unit [@@deriving compare, hash, sexp] *)

(*     let default = () *)
(*     let output = default *)
(*   end *)

(*   module Op = struct *)
(*     type t = int [@@deriving compare, hash, sexp] *)

(*     let default = -1 *)
(*     let cost _ = 1 *)
(*     let arity = Fun.id *)
(*     let args_type op = List.init (arity op) ~f:(fun _ -> Type.default) *)
(*     let ret_type _ = Type.default *)
(*     let pp = Fmt.int *)
(*     let is_commutative op = op = 2 *)
(*   end *)

(*   module Value = struct *)
(*     type t = unit [@@deriving compare, hash, sexp] *)

(*     let default = () *)
(*   end *)
(* end *)

(* open Search_state_all.Make (Lang) *)

(* let%expect_test "" = *)
(*   let ss = create () in *)
(*   insert_class ss () 2 *)
