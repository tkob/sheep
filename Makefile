SRC = alpha.sml closure.sml fv.sml gental.sml global.sml shpc.sml parse.sml parsestr.sml scan.ulex.sml set.sml

SHPFRONT_SRC = shpfront.sml

check: shp shpc shpfront
	rm -f log.txt
	prove --exec 't/do-sml-test -log log.txt' t/sml/*.t
	prove --exec 't/do-test -log log.txt' t/*.t

%.sml: %.sml.in
	autom4te -l m4sugar -o $@ $<

shpc: $(SRC) shpc.mlb
	mlton shpc.mlb

shpfront: $(SHPFRONT_SRC) shpfront.mlb
	mlton shpfront.mlb

parse.sml scan.ulex.sml: parse.cf
	proglr -o $@ -l scan.ulex $<

mkresource: mkresource.sml boot.sml mkresource.mlb
	mlton mkresource.mlb

clean:
	rm -f shp shpc parse.sml scan.ulex.sml log.txt

.PHONY: check clean
