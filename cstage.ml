open! Core

module type CODE = sig
  type +'a t

  type 'a scope

  val new_scope : ('a scope -> 'a t) -> 'a t

  val genlet : 'a scope -> 'b t -> 'b t

  val int : int -> int t

  val bool : bool -> bool t

  val array : ('a -> 'a t) -> 'a array -> 'a array t

  val int : int -> int t

  val ( ~- ) : int t -> int t

  val ( + ) : int t -> int t -> int t

  val ( - ) : int t -> int t -> int t

  val ( * ) : int t -> int t -> int t

  val ( / ) : int t -> int t -> int t

  val ( mod ) : int t -> int t -> int t

  val ( > ) : int t -> int t -> bool t

  val ( < ) : int t -> int t -> bool t

  val ( >= ) : int t -> int t -> bool t

  val ( <= ) : int t -> int t -> bool t

  val ( = ) : int t -> int t -> bool t

  val ( && ) : bool t -> bool t -> bool t

  val ( || ) : bool t -> bool t -> bool t

  val not : bool t -> bool t

  val get : 'a array t -> int t -> 'a t

  val set : 'a array t -> int t -> 'a t -> 'a array t

  val fold : 'a array t -> init:'b t -> f:('b t -> 'a t -> 'b t) -> 'b t

  val sub : 'a array t -> int t -> int t -> 'a array t

  val init : int t -> (int t -> 'a t) -> 'a array t

  val ite : bool t -> 'a t -> 'a t -> 'a t

  val range : int t -> (int t -> unit t) -> unit t

  val seq : unit t -> unit t -> unit t
end

module type DYN_ARRAY = sig
  type +'a code

  type 'a t

  val create : int code -> 'a t code

  val get : 'a t code -> int code -> 'a code

  val set : 'a t code -> int code -> 'a code -> 'a t code
end

module type STA_ARRAY = sig
  type +'a code

  type 'a t

  val create : int -> 'a t code

  val length : 'a t code -> int code

  val get : 'a t code -> int code -> 'a code

  val set : 'a t code -> int code -> 'a code -> 'a t code
end
