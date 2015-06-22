RESULT = espresso

SOURCES = \
	espresso.ml \
	tests.ml 

PACKS = unix oUnit

all: byte-code tests

tests: byte-code
	./espresso

include OCamlMakefile
	

