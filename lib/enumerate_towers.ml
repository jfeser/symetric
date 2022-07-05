open Std

let dim = 32

module Tower = struct
  include Tower

  module Value = struct
    module Block = struct
      module T = struct
        type t = { x : int; y : int; kind : int } [@@deriving compare, equal, hash, sexp]

        let compare b b' =
          match [%compare: int] b.x b'.x with
          | 0 -> (
              match [%compare: int] b.y b'.y with
              | 0 -> [%compare: int] b.kind b'.kind
              | c -> c)
          | c -> c
      end

      include T
      include Comparator.Make (T)

      let is_horiz b = b.kind = 0
      let is_vert b = b.kind = 1
    end

    module Block_set = struct
      type t = Block.t list [@@deriving compare, equal, hash, sexp]

      let add bs b =
        let[@tail_mod_cons] rec insert b = function
          | [] -> [ b ]
          | b' :: bs ->
              if [%compare: Block.t] b b' <= 0 then b :: b' :: bs else b' :: insert b bs
        in
        insert b bs

      let empty = []
      let mem bs b = List.mem ~equal:[%equal: Block.t] bs b

      let zero : t -> _ = function
        | [] -> []
        | b :: bs ->
            let min_x = b.x in
            let min_y = b.y in
            List.map (b :: bs) ~f:(fun b -> { b with x = b.x - min_x; y = b.y - min_y })

      let pp dim fmt blocks =
        let is_h b x y = mem b Block.{ x; y; kind = 0 } in
        let is_v b x y = mem b Block.{ x; y; kind = 1 } in
        let is_on b x y =
          is_h b x y
          || is_h b (x - 1) y
          || is_h b (x - 2) y
          || is_v b x y
          || is_v b x (y - 1)
          || is_v b x (y - 2)
        in

        Fmt.pf fmt "@[<v>";
        for j = dim - 1 downto 0 do
          for i = 0 to dim - 1 do
            if is_on blocks i j then Fmt.pf fmt "█" else Fmt.pf fmt "."
          done;
          Fmt.pf fmt "@,"
        done;
        Fmt.pf fmt "@]"
    end

    module State = struct
      type t = { hand : int; tops : Small_int_array.t; blocks : Block_set.t }
      [@@deriving compare, equal, hash, sexp]

      let default dim =
        {
          hand = 0;
          tops = Small_int_array.init (dim + 2) ~f:(fun _ -> 0);
          blocks = Block_set.empty;
        }
    end

    type transition = {
      prog : Op.t Program.t;
      func : State.t -> State.t;
      summary : State.t list;
    }
    [@@deriving sexp]

    let compare_transition t t' =
      [%compare: State.t] (t.func @@ State.default dim) (t'.func @@ State.default dim)

    let equal_transition t t' =
      [%equal: State.t] (t.func @@ State.default dim) (t'.func @@ State.default dim)

    let hash_transition t = [%hash: State.t] (t.func @@ State.default dim)
    let hash_fold_transition s t = [%hash_fold: State.t] s (t.func @@ State.default dim)

    type t = Int of int | Trans of transition [@@deriving compare, equal, hash, sexp]

    let default = Int (-1)

    module Ctx = struct
      type t = {
        summary_states : State.t list;
        dim : int;
        slices : (Block_set.t * int) list;
      }
      [@@deriving sexp]

      let slices dim (blocks : Block_set.t) =
        let v_slices_l =
          Iter.int_range ~start:0 ~stop:(dim - 1)
          |> Iter.filter (fun min_x ->
                 not
                   (List.exists
                      ~f:(fun b ->
                        Block.is_horiz b
                        && (min_x = b.x || min_x - 1 = b.x || min_x - 2 = b.x))
                      blocks))
          |> Iter.map (fun min_x ->
                 let blocks' = List.filter blocks ~f:(fun b -> min_x <= b.x) in
                 (blocks', List.length blocks - List.length blocks'))
          |> Iter.to_list
          |> List.dedup_and_sort ~compare:[%compare: Block_set.t * int]
          |> List.map ~f:(fun (b, d) -> (Block_set.zero b, d))
        in
        let v_slices_r =
          Iter.int_range ~start:0 ~stop:(dim - 1)
          |> Iter.filter (fun max_x ->
                 not
                   (List.exists
                      ~f:(fun b ->
                        Block.is_horiz b
                        && (max_x = b.x || max_x - 1 = b.x || max_x - 2 = b.x))
                      blocks))
          |> Iter.map (fun max_x ->
                 let blocks' = List.filter blocks ~f:(fun b -> max_x >= b.x) in
                 (blocks', List.length blocks - List.length blocks'))
          |> Iter.to_list
          |> List.dedup_and_sort ~compare:[%compare: Block_set.t * int]
          |> List.map ~f:(fun (b, d) -> (Block_set.zero b, d))
        in
        v_slices_l @ v_slices_r

      let create ?(dim = 32) ?(summary_states = [ State.default dim ]) ?target () =
        let slices = Option.map target ~f:(slices dim) |> Option.value ~default:[] in
        { summary_states; dim; slices }
    end

    let pp (ctx : Ctx.t) fmt (s : State.t) =
      Fmt.pf fmt "@[<v>";
      Fmt.pf fmt "%a@," Sexp.pp_hum ([%sexp_of: State.t] s);
      for i = 0 to ctx.dim - 1 do
        if i = s.hand then Fmt.pf fmt "v" else Fmt.pf fmt " "
      done;
      Fmt.pf fmt "@,";
      Block_set.pp ctx.dim fmt s.blocks;
      Fmt.pf fmt "@]"

    let empty = Set.empty (module Int)

    let eval_unmemoized (ctx : Ctx.t) (op : Op.t) args : t =
      let fail () =
        raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)]
      in
      let prog =
        let arg_progs =
          List.map args ~f:(function Trans t -> t.prog | Int x -> Apply (Op.Int x, []))
        in
        P.Apply (op, arg_progs)
      in
      let trans func =
        Trans
          {
            func;
            summary =
              (let s0 = State.default ctx.dim in
               let s1 = func s0 in
               let s2 = func s1 in
               let s3 = func s2 in
               [ s1; s2; s3 ]);
            prog;
          }
      in
      match (op, args) with
      | Int _, _ :: _ -> fail ()
      | Int x, [] -> Int x
      | (Drop_h | Drop_v), _ :: _ -> fail ()
      | Drop_h, [] ->
          let func (s : State.t) : State.t =
            if s.hand < 0 || s.hand > ctx.dim - 2 then s
            else
              let v_pos =
                Small_int_array.(
                  max (get s.tops s.hand)
                  @@ max (get s.tops (s.hand + 1)) (get s.tops (s.hand + 2)))
              in
              if v_pos >= ctx.dim then s
              else
                let blocks = Block_set.add s.blocks { x = s.hand; y = v_pos; kind = 0 } in
                let tops =
                  Small_int_array.set_many s.tops
                    (Iter.int_range ~start:s.hand ~stop:(min (ctx.dim - 1) (s.hand + 2))
                    |> Iter.map (fun i -> (i, v_pos + 1)))
                in
                { s with blocks; tops }
          in
          trans func
      | Drop_v, [] ->
          let func (s : State.t) : State.t =
            if s.hand < 0 || s.hand > ctx.dim - 2 then s
            else
              let v_pos = Small_int_array.get s.tops s.hand in
              if v_pos >= ctx.dim then s
              else
                let blocks = Block_set.add s.blocks { x = s.hand; y = v_pos; kind = 1 } in
                let tops = Small_int_array.set s.tops s.hand (v_pos + 3) in
                { s with blocks; tops }
          in
          trans func
      | Embed, ([] | [ Int _ ] | _ :: _ :: _) -> fail ()
      | Embed, [ Trans p ] -> trans (fun s -> { (p.func s) with hand = s.hand })
      | Seq, ([] | [ _ ] | [ Int _; _ ] | [ _; Int _ ] | _ :: _ :: _ :: _) -> fail ()
      | Seq, [ Trans t; Trans p ] -> trans (fun s -> t.func s |> p.func)
      | (Move_s _ | Move_p _), ([] | [ Int _ ] | _ :: _ :: _) -> fail ()
      | Move_s x, [ Trans t ] -> trans (fun s -> { (t.func s) with hand = s.hand + x })
      | Move_p x, [ Trans t ] -> trans (fun s -> t.func { s with hand = s.hand + x })
      | Loop, ([] | [ _ ] | [ Trans _; _ ] | [ _; Int _ ] | _ :: _ :: _ :: _) -> fail ()
      | Loop, [ Int x; Trans p ] ->
          let func (s : State.t) : State.t =
            let rec loop x' s = if x' >= x then s else loop (x' + 1) (p.func s) in
            loop 0 s
          in
          trans func
      | _ -> assert false

    let mk_eval_memoized () =
      let module Key = struct
        module T = struct
          type nonrec t = Op.t * t list [@@deriving compare, hash, sexp]
        end

        include T
        include Comparable.Make (T)
      end in
      let tbl = Hashtbl.create (module Key) in
      let find_or_eval (ctx : Ctx.t) op args =
        match Hashtbl.find tbl (op, args) with
        | Some v -> v
        | None ->
            let v = eval_unmemoized ctx op args in
            Hashtbl.set tbl ~key:(op, args) ~data:v;
            v
      in
      find_or_eval

    let eval = eval_unmemoized
    let is_error _ = false

    let zeroed_overlap (s : Block_set.t) (s' : Block_set.t) =
      let rec zeroed_distance left both right d (s : Block_set.t) d' (s' : Block_set.t) =
        match (s, s') with
        | [], [] -> (left, both, right)
        | [], xs -> (left, both, right + List.length xs)
        | xs, [] -> (left + List.length xs, both, right)
        | b :: bs, b' :: bs' ->
            let x0 = b.x - d in
            let x0' = b'.x - d' in
            let cmp =
              match [%compare: int] x0 x0' with
              | 0 -> (
                  match [%compare: int] b.y b'.y with
                  | 0 -> [%compare: int] b.kind b'.kind
                  | c -> c)
              | c -> c
            in

            if cmp < 0 then zeroed_distance (left + 1) both right d bs d' s'
            else if cmp > 0 then zeroed_distance left both (right + 1) d s d' bs'
            else zeroed_distance left (both + 1) right d bs d' bs'
      in

      match (s, s') with
      | [], [] -> (0, 0, 0, 0)
      | [], xs -> (0, 0, List.length xs, 0)
      | xs, [] -> (List.length xs, 0, 0, 0)
      | b :: _, b' :: _ ->
          let min_x = b.x in
          let min_x' = b'.x in
          let x_dist = abs (b.x - b'.x) in
          let l, b, r = zeroed_distance 0 0 0 min_x s min_x' s' in
          (l, b, r, x_dist)

    let blocks_distance s s' =
      let left, inter, right, _ = zeroed_overlap s s' in
      let union = left + inter + right in
      if union = 0 then 0. else 1. -. Float.(of_int inter /. of_int union)

    let distance _ctx v v' =
      match (v, v') with
      | Int x, Int x' -> if x = x' then 0. else Float.infinity
      | Trans x, Trans x' ->
          if [%equal: transition] x x' then 0.
          else (
            assert (List.length x.summary > 0);
            assert (List.length x'.summary > 0);
            List.map2_exn x.summary x'.summary ~f:(fun s s' ->
                blocks_distance s.blocks s'.blocks)
            |> Iter.of_list |> Iter.mean |> Option.value ~default:0.)
      | _, _ -> Float.infinity

    let blocks_distance (ctx : Ctx.t) s s' =
      let left, _, right, x_dist = zeroed_overlap s s' in
      Float.of_int (left + (10 * right)) +. (Float.of_int x_dist /. Float.of_int ctx.dim)

    let target_distance (ctx : Ctx.t) _t = function
      | Int _ -> 1.
      | Trans x ->
          Iter.of_list ctx.slices
          |> Iter.map (fun (slice, dropped) ->
                 Iter.of_list x.summary
                 |> Iter.mapi (fun iter (candidate : State.t) ->
                        Float.of_int dropped
                        +. blocks_distance ctx slice candidate.blocks
                        +. Float.of_int iter))
          |> Iter.concat |> Iter.min |> Option.value ~default:1.
  end
end

open Tower
module Synth = Baseline.Make (Tower)

module Stats = struct
  type t = { runtime : Timer.t } [@@deriving yojson_of]

  let create () = { runtime = Timer.create () }
end

let stats = Stats.create ()

let write_output out m_prog =
  let program_size =
    Option.map m_prog ~f:(fun p -> Float.of_int @@ Program.size p)
    |> Option.value ~default:Float.nan
  in
  let program_json =
    Option.map m_prog ~f:(fun p -> `String (Sexp.to_string @@ Tower.serialize p))
    |> Option.value ~default:`Null
  in
  let open Yojson in
  let json =
    `Assoc
      [
        ("method", `String "enumeration");
        ("program", program_json);
        ("program_size", `Float program_size);
        ("stats", [%yojson_of: Stats.t] stats);
      ]
  in
  Out_channel.with_file out ~f:(fun ch -> Safe.to_channel ch json)

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve tower problems with enumeration."
    [%map_open
      let out = flag "-out" (required string) ~doc:" output file" in
      fun () ->
        let ctx = Value.Ctx.create () in
        let target =
          Sexp.input_sexp In_channel.stdin
          |> Tower.parse
          |> Program.eval (Value.eval (Value.Ctx.create ()))
        in
        let operators =
          [ Op.Loop; Drop_v; Drop_h; Embed; Seq ]
          @ (Iter.int_range ~start:1 ~stop:8
            |> Iter.map (fun i -> Op.Int i)
            |> Iter.to_list)
          @ (Iter.append
               (Iter.int_range ~start:(-8) ~stop:(-1))
               (Iter.int_range ~start:1 ~stop:8)
            |> Iter.map (fun i -> Iter.of_list [ Op.Move_p i; Op.Move_s i ])
            |> Iter.concat |> Iter.to_list)
        in
        let synth_ctx = Synth.Ctx.create ctx ~verbose:true operators (`Value target) in
        write_output out None;
        Timer.start stats.runtime;
        let synth = new Synth.synthesizer synth_ctx in
        let p = synth#run in
        Timer.stop stats.runtime;
        write_output out p]
