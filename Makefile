.PHONY: dev
dev:
	dune pkg lock
	dune build
	dune tools exec ocamllsp
	dune tools exec ocaml
