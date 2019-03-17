
test: main.native mini-c
	./mini-c --debug test.c

main.native: *.ml*
	ocamlbuild -cflags "-w -40" $@

mini-c:
	ln -s main.native $@

