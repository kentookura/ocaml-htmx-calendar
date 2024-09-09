(lang dune 3.15)

(name forester_cal)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name forester_cal)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   dune
   routes
   calendar
   forester
   tls-eio
   mirage-crypto-rng-eio
   (cohttp-eio (>= 6.0.0~beta2))
   (eio_main (>= 1.1))
   (pure-html (>= 3.6.1))
   (icalendar (>= 0.1.8)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
