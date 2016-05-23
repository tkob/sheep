SRC = alpha.sml closure.sml fv.sml gental.sml global.sml shpc.sml parse.sml parsestr.sml scan.ulex.sml set.sml

SHPFRONT_SRC = shpfront.sml

check: shp shpc shpfront
	rm -f log.txt
	prove --exec 't/do-test -log log.txt'

shp: shp.in
	autom4te -l M4sh -o $@ $<
	perl -i -pne 's/^\s*export\s+(LC_ALL|LANGUAGE)\s*$$//' $@

%.sml: %.sml.in
	autom4te -l m4sugar -o $@ $<

shpc: $(SRC) shpc.mlb
	mlton shpc.mlb

shpfront: $(SHPFRONT_SRC) shpfront.mlb
	mlton shpfront.mlb

parse.sml scan.ulex.sml: parse.cf
	proglr -o $@ -l scan.ulex $<

clean:
	rm -f shp shpc parse.sml scan.ulex.sml log.txt

.PHONY: check clean
