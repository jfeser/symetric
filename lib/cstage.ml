open! Core
module Seq = Sequence

module Code () : Sigs.CODE = struct
  module C = Cstage_core.Make ()

  type 'a t = C.expr [@@deriving sexp_of]

  include C
  module Int = Cstage_int.Int (C)
  module Array = Cstage_array.Array (C)
  module Set = Cstage_set.Set (C)
  module String = Cstage_string.String (C)
  module Tuple = Cstage_tuple.Tuple (C)
end
