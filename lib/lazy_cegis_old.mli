open Ast

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

val synth : Op.t list -> Conc.t -> Search_state.t * Stats.t
