RESULT = espresso

SOURCES = \
	MultipleValued.ml \
  partition.ml \
  tautology.ml \
	espresso.ml \
	expand.ml \
  irredundant.ml \
  reduce.ml \
  MVDebug.ml \
	tests/tests.ml  

PACKS = unix oUnit

all: byte-code tests

tests: byte-code
	./espresso

include OCamlMakefile
	

