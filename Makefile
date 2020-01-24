USER_SML_LIB?=${HOME}/SML

all:
	@echo "make targets: poly mlton clean"
	@echo "make depends && make USER_SML_LIB=lib poly mlton"

poly: os-constants.sml
	env USER_SML_LIB=${USER_SML_LIB} polyc -o t-poly t.mlp

mlton: os-constants.sml
	mlton -mlb-path-var 'USER_SML_LIB ${USER_SML_LIB}' -default-ann 'allowFFI true' -output t-mlton t.mlb

os-constants.sml: os-constants.c
	cc -o os-constants os-constants.c && ./os-constants > os-constants.sml && rm os-constants

depends: lib lib/ev

lib:
	mkdir lib

lib/ev:
	git clone https://github.com/kni/sml-ev.git lib/ev

clean:
	rm -rf lib t-poly t-mlton os-constants.sml
