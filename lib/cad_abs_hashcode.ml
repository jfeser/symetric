include Cad_abs

type hash_code = Ternary.t list [@@deriving compare, hash]

let hash_code (params : (Cad_bench.t, _) Params.t) a =
  let ret = ref [] in
  for y = params.bench.input.ymax - 1 downto 0 do
    for x = 0 to params.bench.input.xmax - 1 do
      let impl =
        Cad_abs.implies a Vector2.{ x = Float.of_int x; y = Float.of_int y }
      in
      ret := impl :: !ret
    done
  done;
  !ret

let search_compare params =
  let module C = struct
    module T = struct
      type nonrec t = t [@@deriving sexp]

      let compare x x' =
        [%compare: hash_code] (hash_code params x) (hash_code params x')

      let hash x = [%hash: hash_code] (hash_code params x)

      let hash_fold_t s x = [%hash_fold: hash_code] s (hash_code params x)
    end

    include T
    include Comparator.Make (T)
  end in
  (module C : Lang_abs_intf.Comparable with type t = t)
