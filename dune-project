(lang dune 2.7)
(allow_approximate_merlin)
(generate_opam_files true)
(package
 (name staged_synth)
 (depends
  (core (>= v0.15))
  ppx_jane
  base_quickcheck
  fmt
  sek
  ppx_yojson_conv
  core_bench
  gen
  bheap
  iter
  logs))
