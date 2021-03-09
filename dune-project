(lang dune 2.7)
(allow_approximate_merlin)
(generate_opam_files true)
(package
 (name staged-synth)
 (depends
  (core_profiler (>= v0.14))
  (core (>= v0.14))
  (ppx_jane (>= v0.14))
  (base_quickcheck (and :with-test (>= v0.14)))
  ocamlgraph
  fmt
  progress))
