all:
	@echo "make targets: poly, mlton, clean."

poly: os-constants.sml ev
	polyc -o t-poly t.mlp

mlton: os-constants.sml ev
	mlton -default-ann 'allowFFI true' -output t-mlton t.mlb

os-constants.sml: os-constants.c
	cc -o os-constants os-constants.c && ./os-constants > os-constants.sml && rm os-constants

ev:
	git clone https://github.com/kni/sml-ev.git ev

clean:
	rm -rf t-poly t-mlton os-constants.sml
	test -h ev || rm -rf ev
