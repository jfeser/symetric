include struct
  open Dumb_params

  let spec = Spec.inherit_ Sampling_diverse.spec "cad-learned"

  let neighbor_sample_size =
    Spec.add spec
    @@ Param.int ~name:"neighbor-sample-size" ~doc:" number of neighbors to retain for metric learning"
         ~init:(`Cli (Some 3)) ()

  let inter_centroid_distance =
    Spec.add spec
    @@ Param.float ~name:"inter-centroid-distance" ~doc:" target distance between cluster centers"
         ~init:(`Cli (Some 1.0)) ()
end

module Lang = Cad
module Parent = Sampling_diverse.Make (Lang)
module Search_state = Search_state_append.Make (Lang)
module Gen = Synth_utils.Generate_list (Lang)

class synthesizer params =
  let open Lang in
  object (self)
    inherit Parent.synthesizer params as super

    val neighbor_sample_size = Params.get params neighbor_sample_size

    val inter_centroid_distance = Params.get params inter_centroid_distance

    val mutable metric = Metric_learn.Lego.create 900

    val mutable centers = []

    method! dist v v' =
      let module Mat = Owl.Mat in
      let module Arr = Owl.Arr in
      try
        let v = Mat.transpose @@ Arr.expand (Value.to_ndarray v) 2
        and v' = Mat.transpose @@ Arr.expand (Value.to_ndarray v') 2 in
        Mat.((Metric_learn.Lego.dist metric v v').%{(0, 0)})
      with exn ->
        print_string (Owl_exception.to_string exn);
        raise exn

    method update_metric center neighbors =
      let module Mat = Owl.Mat in
      let center = Value.to_ndarray center and neighbors = List.map neighbors ~f:Value.to_ndarray in
      let n_close = List.length neighbors and n_far = List.length centers in
      let lhs = Array.init (n_close + n_far) ~f:(Fun.const center) |> Mat.of_rows
      and rhs = Array.of_list (neighbors @ centers) |> Mat.of_rows
      and dists =
        let dists_arr =
          Array.of_list @@ List.init n_close ~f:(Fun.const 0.0) @ List.init n_far ~f:(Fun.const inter_centroid_distance)
        in
        Mat.of_array dists_arr (Array.length dists_arr) 1
      in
      Mat.print lhs;
      metric <- Metric_learn.Lego.update metric lhs rhs dists;
      centers <- center :: centers

    method! search_neighbors ?(view = ignore) center f =
      print_s [%message "searching..."];
      let sampler = Sample.Incremental.reservoir_unique (module Value) neighbor_sample_size in
      super#search_neighbors center
        ~view:(fun v ->
          view v;
          sampler.Sample.Incremental.add v)
        f;
      let neighbors = sampler.get_sample () in
      let center = Program.eval (Value.eval params) center in
      self#update_metric center neighbors
  end

let synth params = Option.iter (new synthesizer params)#run ~f:(fun p -> eprint_s [%message (p : Lang.Op.t Program.t)])

let cli =
  let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; spec ] in
  let open Command.Let_syntax in
  Command.basic
    ~summary:(sprintf "Diversity sampling for %s" Lang.name)
    [%map_open
      let params = Dumb_params.Spec.cli spec in
      Synth_utils.run_synth synth params]
