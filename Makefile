compiled/%.run: compiled/%.o main.c
	clang -o $@ -g -fsanitize=address main.c $<

compiled/%.o: compiled/%.s
	nasm -f elf64 -o $@ $<

compiled/%.s: programs/%.ana
	cabal v2-install --install-method=copy --installdir=. --overwrite-policy=always
	mkdir -p compiled
	./ana-compiler $< > $@

clean:
	rm -rf compiled/*.o compiled/*.s compiled/*.dSYM compiled/*.run *.log

