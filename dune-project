(lang dune 3.8)

(name goto)
(generate_opam_files true)

(source (github tjdevries/goto.ml))
(authors "TJ DeVries")
(maintainers "TJ DeVries")

(license MIT)

(package
 (name goto)
 (synopsis "`goto` in OCaml")
 (description "`goto` synopsis")
 (depends ocaml dune ocaml-lsp-server)
 (tags ("meme")))
