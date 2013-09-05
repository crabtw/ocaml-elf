all:
	obuild configure --enable-tests
	obuild build

install:
	ocamlfind install elf src/META $(wildcard dist/build/lib-elf/*)

uninstall:
	ocamlfind remove elf

test:
	obuild test

clean:
	rm -rf dist
