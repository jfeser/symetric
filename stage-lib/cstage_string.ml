open! Core
open Types

module type S = sig
  type t
  type 'a code
  type 'a ctype

  val type_ : t ctype

  module O : sig
    val ( = ) : t code -> t code -> bool code
  end

  val const : String.t -> t code
  val input : t code
  val print : t code -> unit code
  val of_sexp : sexp code -> t code
  val sexp_of : t code -> sexp code
end

module String (C : Cstage_core.S) = struct
  type t

  open C

  let type_ = Type.create ~name:"std::string"
  let of_sexp = Sexp.to_atom

  module O = struct
    let ( = ) s s' = eformat "($(s)) == ($(s'))" Bool.type_ "" [ ("s", C s); ("s'", C s') ]
  end

  let const s = eformat "$(s)" type_ "" [ ("s", S (sprintf "%S" s)) ]
  let print s = eformat ~has_effect:true "0" unit_t "std::cout << $(str) << std::endl;" [ ("str", C s) ]

  let input =
    eformat ~has_effect:true "$(var)" type_
      {|
std::string $(var);
char $(buf)[4096];
while (std::cin.read($(buf), sizeof($(buf)))) {
  $(var).append($(buf), sizeof($(buf)));
}
$(var).append($(buf), std::cin.gcount());
|}
      [ ("var", S (fresh_name ())); ("buf", S (fresh_name ())) ]

  let sexp_of _ = failwith "unimplemented"
end
