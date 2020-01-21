open! Core

type 'a set

type sexp

module type EXAMPLES = sig
  type value

  val inputs : (string * value) list

  val output : string * value
end

module type SKETCH = sig
  (* TODO *)
  (* val body : Grammar.t *)

  val inputs : Grammar.nonterm list

  val output : Grammar.nonterm
end

module type CACHE = sig
  type t

  type value

  type 'a code

  val empty : (t -> 'a code) -> 'a code

  val put :
    sym:string -> size:int -> sizes:int32 array code -> t -> value -> unit code

  val iter :
    sym:string ->
    size:int32 code ->
    f:(value * int32 array code -> unit code) ->
    t ->
    unit code

  val print_size : t -> unit code
end

module type LANG = sig
  type 'a code

  module Value : sig
    type t

    type type_

    val ( = ) : t -> t -> bool code option

    val eq : t -> 'a code -> bool code

    val code : t -> 'a code

    val let_ : t -> (t -> 'a code) -> 'a code

    val type_of : t -> type_

    val of_sexp : Grammar.nonterm -> sexp code -> t

    type mapper = { f : 'a. 'a code -> 'a code }

    val map : f:mapper -> t -> t
  end

  val grammar : Grammar.t

  val eval : Value.t Map.M(String).t -> _ Grammar.Term.t -> Value.t
end

module type CODE = sig
  type 'a t [@@deriving sexp_of]

  type ctype [@@deriving sexp_of]

  val type_of : 'a t -> ctype

  val to_string : 'a t -> string

  val cast : 'a t -> 'b t

  val genlet : 'a t -> 'a t

  val let_locus : (unit -> 'a t) -> 'a t

  (* Primitive types *)
  val unit_t : ctype

  val int_t : ctype

  val bool_t : ctype

  (* Values *)
  val unit : unit t

  (* Integer operations *)
  module Int : sig
    val type_ : ctype

    val int : int -> int32 t

    val ( ~- ) : int32 t -> int32 t

    val ( + ) : int32 t -> int32 t -> int32 t

    val ( - ) : int32 t -> int32 t -> int32 t

    val ( * ) : int32 t -> int32 t -> int32 t

    val ( / ) : int32 t -> int32 t -> int32 t

    val ( mod ) : int32 t -> int32 t -> int32 t

    val ( > ) : int32 t -> int32 t -> bool t

    val ( < ) : int32 t -> int32 t -> bool t

    val ( >= ) : int32 t -> int32 t -> bool t

    val ( <= ) : int32 t -> int32 t -> bool t

    val ( = ) : int32 t -> int32 t -> bool t

    val min : int32 t -> int32 t -> int32 t

    val max : int32 t -> int32 t -> int32 t

    val of_sexp : sexp t -> int32 t
  end

  (* Boolean operations *)
  module Bool : sig
    val type_ : ctype

    val bool : bool -> bool t

    val ( && ) : bool t -> bool t -> bool t

    val ( || ) : bool t -> bool t -> bool t

    val not : bool t -> bool t

    val of_sexp : sexp t -> bool t
  end

  (* Function operations *)
  module Func : sig
    val type_ : ctype -> ctype -> ctype
  end

  (* Array operations *)
  module Array : sig
    val mk_type : ctype -> ctype

    val elem_type : ctype -> ctype

    module O : sig
      val ( = ) : 'a array t -> 'a array t -> bool t
    end

    val const : ctype -> 'a t array -> 'a array t

    val get : 'a array t -> int32 t -> 'a t

    val set : 'a array t -> int32 t -> 'a t -> unit t

    val length : 'a array t -> int32 t

    val fold : 'a array t -> init:'b t -> f:('b t -> 'a t -> 'b t) -> 'b t

    val iter : 'a array t -> f:('a t -> unit t) -> unit t

    val sub : 'a array t -> int32 t -> int32 t -> 'a array t

    val init : ctype -> int32 t -> (int32 t -> 'a t) -> 'a array t

    val map : ctype -> 'a array t -> f:('a t -> 'b t) -> 'b array t

    val map2 :
      ctype ->
      'a array t ->
      'b array t ->
      f:('a t -> 'b t -> 'c t) ->
      'c array t

    val of_sexp : ctype -> sexp t -> (sexp t -> 'a t) -> 'a array t
  end

  module Set : sig
    val mk_type : ctype -> ctype

    val empty : ctype -> 'a set t

    val add : 'a set t -> 'a t -> unit t

    val iter : 'a set t -> ('a t -> unit t) -> unit t

    val fold : 'a set t -> init:'b t -> f:('b t -> 'a t -> 'b t) -> 'b t

    val of_sexp : ctype -> sexp t -> (sexp t -> 'a t) -> 'a set t
  end

  (* Tuples *)
  module Tuple : sig
    val mk_type : ctype -> ctype -> ctype

    val create : 'a t -> 'b t -> ('a * 'b) t

    val fst : ('a * 'b) t -> 'a t

    val snd : ('a * 'b) t -> 'b t

    val of_sexp : sexp t -> (sexp t -> 'a t) -> (sexp t -> 'b t) -> ('a * 'b) t
  end

  module String : sig
    val type_ : ctype

    module O : sig
      val ( = ) : string t -> string t -> bool t
    end

    val const : string -> string t

    val input : string t

    val print : string t -> unit t

    val of_sexp : sexp t -> string t
  end

  module Sexp : sig
    val type_ : ctype

    val input : sexp t

    val to_list : sexp t -> sexp list t

    module List : sig
      val get : sexp list t -> int32 t -> sexp t

      val length : sexp list t -> int32 t
    end
  end

  (* Control flow *)
  val for_ : int32 t -> int32 t -> int32 t -> (int32 t -> unit t) -> unit t

  val ite : bool t -> (unit -> 'a t) -> (unit -> 'a t) -> 'a t

  val let_ : 'a t -> ('a t -> 'b t) -> 'b t

  val let_global : 'a t -> ('a t -> 'b t) -> 'b t

  val seq : unit t -> unit t -> unit t

  val seq_many : unit t list -> unit t

  (* Functions *)
  val func : string -> ctype -> ('a t -> 'b t) -> ('a -> 'b) t

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  (* Utility *)
  val print : string -> unit t

  val exit : unit t
end
