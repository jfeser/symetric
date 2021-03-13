open Base_quickcheck

module Make (L : Lattice_intf.S) = struct
  let ( => ) a b = (not a) || b

  module L2 = struct
    type t = L.t * L.t [@@deriving compare, sexp, quickcheck]
  end

  module Leq2 = struct
    type t = L.t * L.t [@@deriving compare, sexp, quickcheck]

    let quickcheck_generator =
      let open Generator.Let_syntax in
      let%bind b = [%quickcheck.generator: L.t] in
      let%bind a = L.quickcheck_generator_leq b in
      return (a, b)
  end

  module Leq3 = struct
    type t = L.t * L.t * L.t [@@deriving compare, sexp, quickcheck]

    let quickcheck_generator =
      let open Generator.Let_syntax in
      let%bind c = [%quickcheck.generator: L.t] in
      let%bind b = L.quickcheck_generator_leq c in
      let%bind a = L.quickcheck_generator_leq b in
      return (a, b, c)
  end

  module L3 = struct
    type t = L.t * L.t * L.t [@@deriving compare, sexp, quickcheck]
  end

  let examples = [ L.top; L.bot ]

  let%test_unit "leq refl: forall a. a <= a" =
    Test.run_exn ~examples ~f:([%test_pred: L.t] (fun a -> L.leq a a)) (module L)

  let%test_unit {|leq transitive: forall a,b,c. (a <= b) /\ (b <= c) ==> (a <= c)|}
      =
    Test.run_exn
      ~f:([%test_pred: L.t * L.t * L.t] (fun (a, _, c) -> L.leq a c))
      (module Leq3)

  let%test_unit {|top is upper bound: forall a. a <= top|} =
    Test.run_exn ~examples
      ~f:([%test_pred: L.t] (fun a -> L.leq a L.top))
      (module L)

  let%test_unit {|bottom is lower bound: forall a. bot <= a|} =
    Test.run_exn ~examples
      ~f:([%test_pred: L.t] (fun a -> L.leq L.bot a))
      (module L)

  let%test_unit {|join commutes: forall a, b. a \/ b = b \/ a|} =
    Test.run_exn
      ~f:(fun (a, b) -> [%test_result: L.t] ~expect:(L.lub b a) @@ L.lub a b)
      (module L2)

  let%test_unit {|join associates: forall a,b,c. (a \/ b) \/ c = a \/ (b \/ c)|}
      =
    Test.run_exn
      ~f:(fun (a, b, c) ->
        [%test_result: L.t] ~expect:(L.lub (L.lub a b) c) @@ L.lub a (L.lub b c))
      (module L3)

  let%test_unit {|join idempotent: forall a. a \/ a = a|} =
    Test.run_exn ~examples
      ~f:(fun a -> [%test_result: L.t] ~expect:a @@ L.lub a a)
      (module L)

  let%test_unit {|meet commutes: forall a, b. a /\ b = b /\ a|} =
    Test.run_exn
      ~f:(fun (a, b) -> [%test_result: L.t] ~expect:(L.glb b a) @@ L.glb a b)
      (module L2)

  let%test_unit {|meet associates: forall a,b,c. (a /\ b) /\ c = a /\ (b /\ c)|}
      =
    Test.run_exn
      ~f:(fun (a, b, c) ->
        [%test_result: L.t] ~expect:(L.glb (L.glb a b) c) @@ L.glb a (L.glb b c))
      (module L3)

  let%test_unit {|meet idempotent: forall a. a /\ a = a|} =
    Test.run_exn ~examples
      ~f:(fun a -> [%test_result: L.t] ~expect:a @@ L.glb a a)
      (module L)

  let%test_unit {|forall a. a \/ top = top|} =
    Test.run_exn
      ~f:(fun a -> [%test_result: L.t] ~expect:L.top @@ L.lub a L.top)
      (module L)

  let%test_unit {|forall a. a /\ top = a|} =
    Test.run_exn
      ~f:(fun a -> [%test_result: L.t] ~expect:a @@ L.glb a L.top)
      (module L)

  let%test_unit {|forall a. a \/ bot = a|} =
    Test.run_exn
      ~f:(fun a -> [%test_result: L.t] ~expect:a @@ L.lub a L.bot)
      (module L)

  let%test_unit {|forall a. a /\ bot = bot|} =
    Test.run_exn
      ~f:(fun a -> [%test_result: L.t] ~expect:L.bot @@ L.glb a L.bot)
      (module L)

  let%test_unit {|join absorbs meet: forall a,b. a \/ (a /\ b) = a|} =
    Test.run_exn
      ~f:(fun (a, b) -> [%test_result: L.t] ~expect:a @@ L.lub a (L.glb a b))
      (module L2)

  let%test_unit {|meet absorbs join: forall a,b. a /\ (a \/ b) = a|} =
    Test.run_exn
      ~f:(fun (a, b) -> [%test_result: L.t] ~expect:a @@ L.glb a (L.lub a b))
      (module L2)

  let%test_unit {|leq/join compat: forall a,b. a <= b ==> a \/ b = b|} =
    Test.run_exn
      ~f:(fun (a, b) -> [%test_result: L.t] ~expect:b @@ L.lub a b)
      (module Leq2)

  let%test_unit {|join/leq compat: forall a,b. a \/ b = b ==> a <= b|} =
    Test.run_exn
      ~f:(fun (a, b) ->
        if [%compare.equal: L.t] (L.lub a b) b then
          [%test_pred: L.t * L.t] (fun (a, b) -> L.leq a b) (a, b))
      (module L2)

  let%test_unit {|leq/meet compat: forall a,b. a <= b ==> a /\ b = a|} =
    Test.run_exn
      ~f:(fun (a, b) -> [%test_result: L.t] ~expect:a @@ L.glb a b)
      (module Leq2)

  (* let join_compat_meet =
   *   (\* forall a,b. a \/ b = b  ==>  a /\ b  = a  *\)
   *   mk_test ~n:1000 ~pp:pp_pair ~limit:1
   *     ~name:("join compatible meet in " ^ L.name) ~size:size_pair ord_pair
   *     (\*arb_pair*\) (fun (a, b) ->
   *       Prop.assume L.(eq (join a b) b);
   *       L.(eq (meet a b) a))
   * 
   * let meet_compat_join =
   *   (\* forall a,b. a /\ b  = a  ==>  a \/ b = b    *\)
   *   mk_test ~n:1000 ~pp:pp_pair ~limit:1
   *     ~name:("meet compatible join in " ^ L.name) ~size:size_pair ord_pair
   *     (\*arb_pair*\) (fun (a, b) ->
   *       Prop.assume L.(eq (meet a b) a);
   *       L.(eq (join a b) b))
   * 
   * let meet_compat_leq =
   *   (\* forall a,b. a /\ b  = a  ==>  a <= b  *\)
   *   mk_test ~n:1000 ~pp:pp_pair ~limit:1
   *     ~name:("meet compatible leq in " ^ L.name) ~size:size_pair ord_pair
   *     (\*arb_pair*\) (fun (a, b) ->
   *       Prop.assume L.(eq (meet a b) a);
   *       L.leq a b)
   * 
   * let leq_compat_meet =
   *   (\* forall a,b. a <= b  ==>  a /\ b  = a  *\)
   *   mk_test ~n:1000 ~pp:pp_pair ~limit:1
   *     ~name:("leq compatible meet in " ^ L.name) ~size:size_pair ord_pair
   *     (\*arb_pair*\) (fun (a, b) ->
   *       Prop.assume (L.leq a b);
   *       L.(eq (meet a b) a)) *)
end
