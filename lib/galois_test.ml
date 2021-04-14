(* open Base_quickcheck
 * 
 * module Make (G : Galois.S) = struct
 *   open G
 *   module UL = Lattice_utils.Make (G.L)
 *   module UM = Lattice_utils.Make (G.M)
 * 
 *   let%test_unit {|lift monotonic: forall l. l \leq_L l' => lift(l) \leq_M lift(l')|}
 *       =
 *     Test.run_exn
 *       ~f:([%test_pred: UL.Leq2.t] (fun (l, l') -> M.leq (lift l) (lift l')))
 *       (module UL.Leq2)
 * 
 *   let%test_unit {|lower monotonic: forall m. m \leq_M m' => lower(m) \leq_L lower(m')|}
 *       =
 *     Test.run_exn
 *       ~f:([%test_pred: UM.Leq2.t] (fun (m, m') -> L.leq (lower m) (lower m')))
 *       (module UM.Leq2)
 * 
 *   let%test_unit "forall m. lift(lower(m)) \leq_M m" =
 *     Test.run_exn ~examples:[ M.top; M.bot ]
 *       ~f:([%test_pred: M.t] (fun m -> M.leq (lift (lower m)) m))
 *       (module M)
 * 
 *   let%test_unit "forall l. lower(lift(l)) \geq_L l" =
 *     Test.run_exn ~examples:[ L.top; L.bot ]
 *       ~f:([%test_pred: L.t] (fun l -> L.leq l (lower (lift l))))
 *       (module L)
 * end *)
