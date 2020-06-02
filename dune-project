(lang dune 2.5)
(allow_approximate_merlin)
(generate_opam_files true)

(package
  (name staged-synth)
  (depends
    (core (>= v0.13))
    (ppx_compare (>= v0.13))
    (ppx_hash (>= v0.13))
    (ppx_sexp_conv (>= v0.13))
    (ppx_let (>= v0.13))
    ocamlgraph
    fmt
    ))
