SOURCE = asm.ml basicasm.ml main.ml
LIBRARY = str.cmxa

all: main

main:
	ocamlopt -o sam $(LIBRARY) $(SOURCE)
