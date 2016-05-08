SRC = alpha.sml closure.sml fv.sml gental.sml global.sml shpc.sml parse.sml parsestr.sml scan.ulex.sml set.sml

check: shp shpc
	rm -f log.txt
	prove --exec 't/do-test -log log.txt'

shp: shp.in
	autom4te -l M4sh -o $@ $<
	perl -i -pne 's/^\s*export\s+(LC_ALL|LANGUAGE)\s*$$//' $@

shpc: $(SRC) shpc.mlb
	mlton shpc.mlb

parse.sml scan.ulex.sml: parse.cf
	proglr -o $@ -l scan.ulex $<

clean:
	rm -f shp shpc parse.sml scan.ulex.sml log.txt

.PHONY: check clean
