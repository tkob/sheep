SRC = alpha.sml closure.sml fv.sml gental.sml global.sml sheepc.sml parse.sml parsestr.sml scan.ulex.sml set.sml

check: sheep sheepc
	runtest

sheep: sheep.in
	autom4te -l M4sh -o $@ $<

sheepc: $(SRC) sheepc.mlb
	mlton sheepc.mlb

parse.sml scan.ulex.sml: parse.cf
	proglr -o $@ -l scan.ulex $<

clean:
	rm -f sheep sheepc parse.sml scan.ulex.sml

.PHONY: check clean
