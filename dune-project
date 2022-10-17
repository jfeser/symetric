(lang dune 2.7)
(generate_opam_files true)
(package
 (name symetric)
 (synopsis "tool")
 (maintainers jack.feser@gmail.com)
 (depends
  (core (>= v0.15))
  menhir
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

(using menhir 2.1)
