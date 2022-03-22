open! Core
open Types

module type EXAMPLES = sig
  type value

  val inputs : (string * value) list
  val output : string * value
end

module type SKETCH = sig
  (* TODO *)
  (* val body : Grammar.t *)

  val background : Grammar.nonterm list
  val input : Grammar.nonterm
  val output : Grammar.nonterm
end

module type CACHE = sig
  type t
  type value
  type 'a code
  type cache

  val code_of : t -> cache code
  val of_code : cache code -> t
  val empty : unit -> (t, 'a code) Nonlocal_let.t
  val put : sym:string -> size:Int.t -> t -> value -> unit code
  val iter : sym:string -> size:int code -> f:(value -> unit code) -> t -> unit code
end

module type LANG = sig
  type 'a code

  module Value : sig
    type t
    type value

    val code_of : t -> value code
    val of_code : value code -> t
    val ( = ) : t -> t -> [ `Static of bool | `Dyn of bool code ]
    val of_sexp : Grammar.nonterm -> sexp code -> t
    val sexp_of : t -> sexp code
  end

  val cost : string -> Int.t
  val grammar : (Value.t, bool code) Semantics.t Grammar.t
  val eval : Value.t Map.M(String).t -> Grammar.Untyped_term.t -> Value.t
end

module type CODE = sig
  type 'a t [@@deriving sexp_of]
  type 'a ctype [@@deriving sexp_of]

  val type_of : 'a t -> 'a ctype
  val add_annot : 'a t -> 'b Univ_map.Key.t -> 'b -> 'a t
  val find_annot : 'a t -> 'b Univ_map.Key.t -> 'b option
  val to_string : 'a t -> string
  val cast : 'a t -> 'b t
  val genlet : 'a t -> 'a t
  val let_locus : (unit -> 'a t) -> 'a t

  (* Unit *)
  val unit_t : unit ctype
  val unit : unit t

  (* Boolean operations *)
  module Bool : sig
    val type_ : bool ctype
    val bool : bool -> bool t
    val ( && ) : bool t -> bool t -> bool t
    val ( || ) : bool t -> bool t -> bool t
    val not : bool t -> bool t
    val of_sexp : sexp t -> bool t
    val sexp_of : bool t -> sexp t
  end

  (* Function operations *)
  module Func : sig
    val mk_type : 'a ctype -> 'b ctype -> ('a -> 'b) ctype
    val apply : ('a -> 'b) t -> 'a t -> 'b t
    val func : string -> ('a -> 'b) ctype -> ('a t -> 'b t) -> ('a -> 'b) t
  end

  module Int :
    Cstage_int.S with type 'a code := 'a t and type 'a ctype := 'a ctype and type t = int

  module Array : Cstage_array.S with type 'a code := 'a t and type 'a ctype := 'a ctype
  module Set : Cstage_set.S with type 'a code := 'a t and type 'a ctype := 'a ctype
  module String : Cstage_string.S with type 'a code := 'a t and type 'a ctype := 'a ctype

  (* Tuples *)
  module Tuple : sig
    val mk_type : 'a ctype -> 'b ctype -> ('a * 'b) ctype
    val create : 'a t -> 'b t -> ('a * 'b) t
    val fst : ('a * 'b) t -> 'a t
    val snd : ('a * 'b) t -> 'b t
    val of_sexp : sexp t -> (sexp t -> 'a t) -> (sexp t -> 'b t) -> ('a * 'b) t
    val sexp_of : ('a * 'b) t -> ('a t -> sexp t) -> ('b t -> sexp t) -> sexp t
  end

  module Sexp : sig
    val type_ : sexp ctype
    val input : unit -> sexp t
    val print : sexp t -> unit t
    val to_list : sexp t -> sexp list t

    module List : sig
      val get : sexp list t -> int t -> sexp t
      val length : sexp list t -> int t
    end
  end

  (* Control flow *)
  val for_ : int t -> int t -> int t -> (int t -> unit t) -> unit t
  val ite : bool t -> (unit -> 'a t) -> (unit -> 'a t) -> 'a t
  val let_ : 'a t -> ('a t -> 'b t) -> 'b t
  val let_global : 'a t -> ('a t -> 'b t) -> 'b t
  val seq : unit t -> unit t -> unit t
  val sseq : unit t list -> unit t

  (* Utility *)
  val print : string -> unit t
  val eprint : string -> unit t
  val exit : unit t
  val return : unit t
end
