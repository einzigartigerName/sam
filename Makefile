SOURCE = asm.ml basicasm.ml main.ml
LIBRARY = str.cmxa

all: clean main

main:
	ocamlopt -o sam $(LIBRARY) $(SOURCE)

clean:
	rm -f asm.cmi asm.cmo