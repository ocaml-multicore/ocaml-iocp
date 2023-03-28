(lang dune 2.9)
(name iocp)
(generate_opam_files true)
(source (github patricoferris/ocaml-iocp))
(license "MIT")
(authors "Patrick Ferris")
(maintainers "patrick@sirref.org")
(package
 (name iocp)
 (synopsis "OCaml bindings for Windows IOCP")
 (description "Bindings to the Windows IOCP API. See https://github.com/ocaml-multicore/eio for a higher-level API using this.")
 (depends
   cstruct
   optint
   (mdx :with-test)
   (alcotest :with-test)
   (bechamel :with-test)))
