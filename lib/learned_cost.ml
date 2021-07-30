(* include struct
 *   open Dumb_params
 * 
 *   let spec = Spec.inherit_ Sampling_diverse.spec "cad-learned"
 * 
 *   let neighbor_sample_size =
 *     Spec.add spec
 *     @@ Param.int ~name:"neighbor-sample-size" ~doc:" number of neighbors to retain for metric learning"
 *          ~init:(`Cli (Some 3)) ()
 * 
 *   let inter_centroid_distance =
 *     Spec.add spec
 *     @@ Param.float ~name:"inter-centroid-distance" ~doc:" target distance between cluster centers"
 *          ~init:(`Cli (Some 200.0)) ()
 * end
 * 
 * module Lang = Cad
 * module Parent = Sampling_diverse.Make (Lang)
 * module Search_state = Search_state_append.Make (Lang)
 * module Gen = Synth_utils.Generate_list (Lang)
 * 
 * exception Reset
 * 
 * class synthesizer params =
 *   let open Lang in
 *   object (self)
 *     inherit Parent.synthesizer params as super
 * 
 *     val neighbor_sample_size = Params.get params neighbor_sample_size
 * 
 *     val inter_centroid_distance = Params.get params inter_centroid_distance
 * 
 *     val mutable metric = Metric_learn.Lego.create 900
 * 
 *     val mutable centers = Map.empty (module Value)
 * 
 *     method to_features v =
 *       let module Mat = Owl.Mat in
 *       let module Arr = Owl.Arr in
 *       Mat.transpose @@ Arr.expand (Value.to_ndarray v) 2
 * 
 *     method! dist v v' =
 *       let module Mat = Owl.Mat in
 *       try
 *         let v = self#to_features v and v' = self#to_features v' in
 *         let d = Metric_learn.Lego.dist metric v v' in
 *         d
 *       with exn ->
 *         print_string (Owl_exception.to_string exn);
 *         raise exn
 * 
 *     method update_metric center neighbors =
 *       let module Mat = Owl.Mat in
 *       let center_m = self#to_features center and neighbors_m = List.map neighbors ~f:self#to_features in
 * 
 *       let metric' = List.fold_left ~init:metric ~f:(fun m n -> Metric_learn.Lego.update m center_m n 0.0) neighbors_m in
 *       let metric' =
 *         Map.fold ~init:metric'
 *           ~f:(fun ~key:v ~data:c m ->
 *             if [%compare.equal: Value.t] v center then m
 *             else Metric_learn.Lego.update m center_m c inter_centroid_distance)
 *           centers
 *       in
 * 
 *       (\* Fmt.pr "Center:\n%a\n" Value.pprint center;
 *        * List.iter neighbors ~f:(Fmt.pr "Neighbor:\n%a\n" Value.pprint);
 *        * Map.to_alist centers |> List.iter ~f:(fun (v, _) -> Fmt.pr "Alt center:\n%a\n" Value.pprint v); *\)
 *       metric <- metric';
 *       centers <- Map.update centers center ~f:(function Some c -> c | None -> center_m)
 * 
 *     method! search_neighbors ?(view = ignore) center f =
 *       print_s [%message "searching..."];
 *       let sampler = Sample.Incremental.reservoir_unique (module Value) neighbor_sample_size in
 *       let center_v = Program.eval (Value.eval params) center in
 * 
 *       Fmt.pr "Center:\n%a\n" Value.pprint center_v;
 * 
 *       super#search_neighbors center
 *         ~view:(fun v ->
 *           view v;
 *           if not @@ [%compare.equal: Value.t] v center_v then sampler.Sample.Incremental.add v)
 *         f;
 *       let neighbors = sampler.get_sample () in
 *       self#update_metric center_v neighbors
 * 
 *     method! run = try super#run with Reset -> self#run
 *   end
 * 
 * let synth params = Option.iter (new synthesizer params)#run ~f:(fun p -> eprint_s [%message (p : Lang.Op.t Program.t)])
 * 
 * let cli =
 *   let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; spec ] in
 *   let open Command.Let_syntax in
 *   Command.basic
 *     ~summary:(sprintf "Diversity sampling for %s" Lang.name)
 *     [%map_open
 *       let params = Dumb_params.Spec.cli spec in
 *       Synth_utils.run_synth synth params] *)
