# compile: compile.ml
# 	ocamlfind ocamlc -o compile -thread -package oUnit -package sexplib -linkpkg -g compile.ml
#
# %.run: %.o
# 	clang -o $@ main.c $<
#
# %.o: %.s
# 	nasm -f macho64 -o $@ $<
#
# %.s: %.int compile
# 	./compile $< > $@


compile: app/Main.hs
	cabal v2-install --install-method=copy --installdir=. --overwrite-policy=always

%.run: %.o
	clang -o $@ main.c $<

%.o: %.s
	nasm -f elf64 -o $@ $<

%.s: %.int compile
	./compile $< > $@
