include struct
  open Dumb_params

  let spec = Spec.inherit_ Sampling_diverse.spec "sampling-diverse"

  let synth = Spec.add spec @@ Param.const_str ~name:"synth" "sampling-diverse-nn"
end

let (_ : bool) = Torch.Tensor.grad_set_enabled false

include struct
  module Lang = Cad
  module Parent = Sampling_diverse
  module Search_state = Parent.Search_state
  open Lang

  let rec batched n l =
    let b, bs = List.split_n l n in
    if List.is_empty bs then [ b ] else b :: batched n bs

  let sample_batched ?(batch_size = 1024) embed retain_thresh states old new_ =
    let open Torch in
    if List.is_empty old || List.is_empty new_ then new_
    else
      batched batch_size new_
      |> List.concat_map ~f:(fun new_ ->
             let device = Device.Cuda 0 in
             let to_tensor s =
               Tensor.to_device ~device @@ Tensor.unsqueeze ~dim:0 @@ Tensor.squeeze @@ embed
               @@ List.map s ~f:(fun i -> states.(i))
             in
             let old_tensor = to_tensor old and new_tensor = to_tensor new_ in
             let dists = Tensor.squeeze @@ Tensor.cdist ~x1:old_tensor ~x2:new_tensor ~p:1.0 ~compute_mode:0 in
             let min_dists = Tensor.amin dists ~dim:[ 0 ] ~keepdim:false in
             let mask = Tensor.ge min_dists @@ Scalar.f retain_thresh in
             let indices =
               Tensor.range ~start:(Scalar.i 0)
                 ~end_:(Scalar.i @@ (List.length new_ - 1))
                 ~options:(Torch_core.Kind.T Int, device)
             in
             let retained_indices = Tensor.masked_select indices ~mask in
             Tensor.to_int1_exn retained_indices |> Array.to_list)

  let find_close_states ?(batch_size = 2048) embed search_thresh output states new_ =
    let open Torch in
    if List.is_empty new_ then []
    else
      batched batch_size new_
      |> List.concat_map ~f:(fun new_ ->
             let device = Device.Cuda 0 in
             let e_states =
               Tensor.to_device ~device @@ Tensor.unsqueeze ~dim:0 @@ embed @@ List.map ~f:(fun i -> states.(i)) new_
             and e_output = Tensor.to_device ~device @@ Tensor.unsqueeze ~dim:0 @@ embed [ output ] in

             let dists = Tensor.squeeze @@ Tensor.cdist ~x1:e_output ~x2:e_states ~p:1.0 ~compute_mode:0 in

             let min_dist = Tensor.minimum dists |> Tensor.to_float0_exn in
             let max_dist = Tensor.maximum dists |> Tensor.to_float0_exn in
             print_s [%message (min_dist : float) (max_dist : float)];
             let mask = Tensor.le dists @@ Scalar.f search_thresh in
             let indices =
               Tensor.range ~start:(Scalar.i 0)
                 ~end_:(Scalar.i @@ (List.length new_ - 1))
                 ~options:(Torch_core.Kind.T Int, device)
             in
             let retained_indices = Tensor.masked_select indices ~mask |> Tensor.to_int1_exn |> Array.to_list in
             let retained_dists = Tensor.masked_select dists ~mask |> Tensor.to_float1_exn |> Array.to_list in
             let ret = List.zip_exn retained_dists retained_indices in
             ret)

  class synthesizer params =
    object
      inherit Parent.synthesizer params

      val embed = Value.embed params

      method! find_close_states new_states =
        let new_states_a = Array.of_list new_states in
        let states = Array.of_list @@ List.map ~f:(fun (v, _, _) -> v) new_states in
        List.range 0 (List.length new_states)
        |> find_close_states embed search_thresh _output states
        |> List.map ~f:(fun (d, i) ->
               let v, op, args = new_states_a.(i) in
               (d, v, op, args))

      method! sample_diverse_states new_states =
        let new_states_a = Array.of_list new_states in
        let all_states =
          Array.of_list @@ List.map new_states ~f:(fun (v, _, _) -> v) @ Search_state.states search_state
        in
        let old = List.range (List.length new_states) (Array.length all_states)
        and new_ = List.range 0 (List.length new_states) in
        let idxs = sample_batched embed retain_thresh all_states old new_ in
        List.map idxs ~f:(fun i -> new_states_a.(i))
    end
end

let cli =
  let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; spec ] in
  let open Command.Let_syntax in
  Command.basic ~summary:(sprintf "Diversity sampling (with neural networks) for %s" Lang.name)
  @@ [%map_open
       let params = Dumb_params.Spec.cli spec in
       Synth_utils.run_synth
         (fun params -> new synthesizer params)
         params
         (Option.iter ~f:(fun p -> eprint_s [%message (p : Lang.Op.t Program.t)]))]
