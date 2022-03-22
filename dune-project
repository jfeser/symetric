(lang dune 2.7)
(allow_approximate_merlin)
(generate_opam_files true)
(package
 (name staged-synth)
 (depends
  (core (and (>= v0.14) (< v0.15)))
  (ppx_jane (>= v0.14))
  (base_quickcheck (and :with-test (>= v0.14)))
  fmt
  sek
  ppx_yojson_conv
  core_bench
  gen
  bheap
  iter
  logs))
