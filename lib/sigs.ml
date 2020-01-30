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

  type cache

  val code_of : t -> cache code

  val of_code : cache code -> t

  val empty : unit -> (t, 'a code) Nonlocal_let.t

  val put : sym:string -> size:int -> t -> value -> unit code

  val iter :
    sym:string -> size:int32 code -> f:(value -> unit code) -> t -> unit code

  val print_size : t -> unit code
end

module type LANG = sig
  type 'a code

  module Value : sig
    type t

    type value

    val code_of : t -> value code

    val of_code : value code -> t

    val ( = ) : t -> t -> bool code option

    val eq : t -> 'a code -> bool code

    val code : t -> 'a code

    val let_ : t -> (t -> 'a code) -> 'a code

    val of_sexp : Grammar.nonterm -> sexp code -> t

    val sexp_of : t -> sexp code

    type mapper = { f : 'a. 'a code -> 'a code }

    val map : f:mapper -> t -> t

    val random : ?state:Random.State.t -> Grammar.nonterm -> int -> t
  end

  val grammar : Grammar.t

  val eval : Value.t Map.M(String).t -> Grammar.Term.t -> Value.t
end

module type INT = sig
  type 'a t

  type 'a ctype

  val type_ : int32 ctype

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

  val sexp_of : int32 t -> sexp t
end

module type ARRAY = sig
  type 'a t

  type 'a array

  type 'a ctype

  val mk_type : 'a ctype -> 'a array ctype

  val elem_type : 'a array ctype -> 'a ctype

  module O : sig
    val ( = ) : 'a array t -> 'a array t -> bool t
  end

  val const : 'a array ctype -> 'a t Array.t -> 'a array t

  val get : 'a array t -> int32 t -> 'a t

  val set : 'a array t -> int32 t -> 'a t -> unit t

  val length : 'a array t -> int32 t

  val fold : 'a array t -> init:'b t -> f:('b t -> 'a t -> 'b t) -> 'b t

  val iter : 'a array t -> f:('a t -> unit t) -> unit t

  val sub : 'a array t -> int32 t -> int32 t -> 'a array t

  val init : 'a array ctype -> int32 t -> (int32 t -> 'a t) -> 'a array t

  val map : 'b array ctype -> 'a array t -> f:('a t -> 'b t) -> 'b array t

  val map2 :
    'c array ctype ->
    'a array t ->
    'b array t ->
    f:('a t -> 'b t -> 'c t) ->
    'c array t

  val of_sexp : 'a array ctype -> sexp t -> (sexp t -> 'a t) -> 'a array t

  val sexp_of : 'a array t -> ('a t -> sexp t) -> sexp t
end

module type SET = sig
  type 'a t

  type 'a ctype

  val mk_type : 'a ctype -> 'a set ctype

  val empty : 'a set ctype -> 'a set t

  val add : 'a set t -> 'a t -> unit t

  val iter : 'a set t -> ('a t -> unit t) -> unit t

  val fold : 'a set t -> init:'b t -> f:('b t -> 'a t -> 'b t) -> 'b t

  val of_sexp : 'a set ctype -> sexp t -> (sexp t -> 'a t) -> 'a set t

  val sexp_of : 'a set t -> ('a t -> sexp t) -> sexp t
end

module type CODE = sig
  type 'a t [@@deriving sexp_of]

  type 'a ctype [@@deriving sexp_of]

  val type_of : 'a t -> 'a ctype

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

  module Int : INT with type 'a t := 'a t and type 'a ctype := 'a ctype

  module Array : ARRAY with type 'a t := 'a t and type 'a ctype := 'a ctype

  module Set : SET with type 'a t := 'a t and type 'a ctype := 'a ctype

  (* Tuples *)
  module Tuple : sig
    val mk_type : 'a ctype -> 'b ctype -> ('a * 'b) ctype

    val create : 'a t -> 'b t -> ('a * 'b) t

    val fst : ('a * 'b) t -> 'a t

    val snd : ('a * 'b) t -> 'b t

    val of_sexp : sexp t -> (sexp t -> 'a t) -> (sexp t -> 'b t) -> ('a * 'b) t

    val sexp_of : ('a * 'b) t -> ('a t -> sexp t) -> ('b t -> sexp t) -> sexp t
  end

  module String : sig
    val type_ : string ctype

    module O : sig
      val ( = ) : string t -> string t -> bool t
    end

    val const : string -> string t

    val input : string t

    val print : string t -> unit t

    val of_sexp : sexp t -> string t

    val sexp_of : string t -> sexp t
  end

  module Sexp : sig
    val type_ : sexp ctype

    val input : unit -> sexp t

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

  val sseq : unit t list -> unit t

  (* Utility *)
  val print : string -> unit t

  val exit : unit t
end
