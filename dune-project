(lang dune 3.20)

(name cornell_advanced_compilers_cs6120)

(generate_opam_files true)

(source
 (github 0xfedcafe/cornell-advanced-compilers))

(authors "Artem <sirar@in.tum.de>")

(maintainers "Artem <sirar@in.tum.de>")

(license GPL-3.0)

(documentation https://url/to/documentation)

(package
 (name cornell_advanced_compilers_cs6120)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   core
   core_unix
   base
   stdio
   ppx_inline_test ; for inline tests
   yojson
   ppx_deriving_yojson
   ppx_sexp_conv
   ppx_compare
   ppx_hash
   ppx_enumerate
 )
 (tags
  ("Cornell Advanced Compilers" "ocaml implementation" "compiler construction" "programming languages")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
