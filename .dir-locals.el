((tuareg-mode
  . ((eval
      . (lsp-register-client
         (make-lsp-client
          :new-connection
          (lsp-stdio-connection
           (lambda () "/home/feser/ocaml-workspace/staged-synth/_opam/bin/ocamllsp"))
          :major-modes '(caml-mode tuareg-mode)
          :priority 10
          :server-id 'ocaml-lsp-server-staged-synth)))

     (lsp-enabled-clients . ('ocaml-lsp-server-staged-synth))
     )
  )
 )

