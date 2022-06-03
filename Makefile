main: src/*
	./build

output/%.run: output/%.o main.c
	clang -o $@ -g -fsanitize=address main.c $<

output/%.o: output/%.s
	nasm -f elf64 -o $@ $<

output/%.s: input/%.ana main
	mkdir -p output
	bin/compile $< > $@

clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log

