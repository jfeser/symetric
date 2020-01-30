open! Core
module Seq = Sequence

module Code (C : Cstage_core.S) :
  Sigs.CODE with type 'a t = 'a C.t and type ctype = C.ctype = struct
  include C
  module Int = Cstage_int.Int (C)
  module Array = Cstage_array.Array (C)
  module Set = Cstage_set.Set (C)
  module String = Cstage_string.String (C)
  module Tuple = Cstage_tuple.Tuple (C)
end
