module Stats : sig
  type t = {
    n_state_nodes : int;
    n_arg_nodes : int;
    n_covered : int;
    n_refuted : int;
    min_width : int;
    max_width : int;
    median_width : int;
    sat : bool;
  }
end

val synth :
  ?no_abstraction:bool -> Conc.t list -> Conc.t -> Search_state.t * Stats.t

val random_io : ?state:Random.State.t -> n:int -> k:int -> Conc.t list * Conc.t

val check_search_space :
  ?n:int -> Conc.t list -> Search_state.G.t -> (unit, Conc.t) result
