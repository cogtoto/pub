# Makefile
build:
	ocamlbuild -use-ocamlfind pi.byte 

clean:
	ocamlbuild -clean

.PHONY:
	build clean

