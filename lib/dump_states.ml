include struct
  open Dumb_params

  let spec = Spec.inherit_ Baseline.spec "dump"
end

open Owl
module Lang = Cad_term
module B = Baseline.Make (Lang)
open B

let uncurry2 f x y = f (x, y)

let curry2 f (x, y) = f x y

let init_2d_triu m n f =
  let x = Mat.empty m n in
  let y = Bigarray.array2_of_genarray x in
  for i = 0 to m - 1 do
    for j = i to n - 1 do
      let v = f i j in
      Bigarray.Array2.unsafe_set y i j v;
      Bigarray.Array2.unsafe_set y j i v
    done
  done;
  x

let to_features v = Arr.expand (Cad_conc.to_ndarray v) 2

class synthesizer params =
  object (self : 'self)
    inherit B.synthesizer params as super

    method! check_states _ = ()

    method program_dist p p' = Float.of_int @@ Tree_dist.zhang_sasha ~eq:[%compare.equal: Lang.Op.t] p p'

    (* method program_dist_star equiv dist p p' =
     *   let ps = equiv p and ps' = equiv p' in
     *   let min = ref Float.infinity in
     *   for i = 0 to Array.length ps - 1 do
     *     for j = i to Array.length ps' - 1 do
     *       min := Float.min !min (dist ps.(i) ps'.(j))
     *     done
     *   done;
     *   !min *)

    method dump_states states =
      let value_cost =
        let module S = Baseline.Make (Cad) in
        let synth = new S.synthesizer params in
        (synth#run : _) |> ignore;
        let ss = synth#get_search_state in
        S.Search_state.cost_of ss
      in

      let eval = Program.eval (Cad.Value.eval params) in
      let values =
        List.map states ~f:eval
        |> List.dedup_and_sort ~compare:[%compare: Cad_conc.t]
        |> List.filter ~f:(fun v -> Option.value (value_cost v) ~default:100 >= max_cost)
      in
      let value_idx = List.mapi values ~f:(fun i v -> (v, i)) |> Hashtbl.of_alist_exn (module Cad_conc) in

      let states = List.filter states ~f:(fun s -> Hashtbl.mem value_idx @@ eval s) in
      let value_of_state =
        List.mapi states ~f:(fun i s -> (i, Hashtbl.find_exn value_idx @@ eval s)) |> Hashtbl.of_alist_exn (module Int)
      in
      let values = Array.of_list values in
      let n_values = Array.length values in

      let states = Array.of_list states in
      let n_states = Array.length states in

      (* let by_value =
       *   List.range 0 n_states
       *   |> List.map ~f:(fun i -> (eval states.(i), i))
       *   |> Map.of_alist_multi (module Cad_conc)
       *   |> Map.map ~f:Array.of_list
       * in *)
      (* let equiv i = Map.find by_value (eval states.(i)) |> Option.value_exn in *)
      let min_dist_mat = Mat.create n_values n_values Float.infinity in
      Dumb_progress.(with_bar (basic_bar (n_states * n_states))) (fun bar ->
          for p = 0 to n_states - 1 do
            for p' = p to n_states - 1 do
              Dumb_progress.update bar ((n_states * p) + p');
              let dist = self#program_dist states.(p) states.(p') in
              Mat.(
                let v = Hashtbl.find_exn value_of_state p and v' = Hashtbl.find_exn value_of_state p' in
                let new_min = Float.min min_dist_mat.%{v; v'} dist in
                min_dist_mat.%{v; v'} <- new_min;
                min_dist_mat.%{v'; v} <- new_min)
            done
          done);
      Npy.write min_dist_mat "dist.npy";

      let features = Array.map values ~f:to_features |> Mat.of_rows in
      Npy.write features "features.npy"

    (* method dump_states states =
     *   let module Mat = Owl.Mat in
     *   let states = Array.of_list states in
     *   let n_states = Array.length states in
     *   let dist_mat =
     *     Dumb_progress.(with_bar (basic_bar (n_states * n_states))) (fun bar ->
     *         Mat.init_2d n_states n_states (fun s s' ->
     *             Dumb_progress.update bar ((n_states * s) + s');
     *             Tree_dist.zhang_sasha ~eq:[%compare.equal: Lang.Op.t]
     *               (Search_state.program_exn search_state states.(s))
     *               (Search_state.program_exn search_state states.(s'))
     *             |> Float.of_int))
     *   in
     *   let eq_mat =
     *     Mat.init_2d n_states n_states (fun s s' ->
     *         if [%compare.equal: Cad.Value.t] states.(s) states.(s') then 1.0 else 0.0)
     *   in
     *   Npy.write dist_mat "dist.npy";
     *   Npy.write eq_mat "eq.npy" *)

    method! run =
      let ret = super#run in
      self#dump_states @@ Search_state.search ~cost:max_cost ~type_:Cad_type.output search_state;
      ret
  end

let synth params = Option.iter (new synthesizer params)#run ~f:(fun p -> eprint_s [%message (p : Lang.Op.t Program.t)])

let cli =
  let spec = Dumb_params.Spec.union [ Cad.spec; Params.spec; spec ] in
  let open Command.Let_syntax in
  Command.basic
    ~summary:(sprintf "Diversity sampling for %s" Lang.name)
    [%map_open
      let params = Dumb_params.Spec.cli spec in
      Synth_utils.run_synth synth params]
