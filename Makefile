SRC = alpha.sml closure.sml fv.sml gental.sml global.sml main.sml parse.sml parsestr.sml scan.ulex.sml set.sml

check: sheep main
	runtest

sheep: sheep.in
	autom4te -l M4sh -o $@ $<

main: $(SRC) main.mlb
	mlton main.mlb

parse.sml scan.ulex.sml: parse.cf
	proglr -o $@ -l scan.ulex $<

clean:
	rm -f sheep main parse.sml scan.ulex.sml

.PHONY: check clean
