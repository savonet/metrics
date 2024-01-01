(lang dune 2.9)
(version 0.1.0)
(name metrics)
(source (github savonet/metrics))
(license GPL-3.0)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)
(use_standard_c_and_cxx_flags false)

(package
 (name metrics)
 (synopsis "Metrics for Liquidsoap")
 (description "Generate graph from Liquidsoap metrics.")
 (depends
   dune
   ptime
   yaml
 )
)
