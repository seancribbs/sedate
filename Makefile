ERL=erl
ERLC=erlc
ERLC_OPT=+debug_info -W -o ebin

all: src_src

ebin:
	mkdir ebin

ebin_tests:
	mkdir ebin_tests

src_src: src/sedate_parse.erl ebin
	cd src;erl -pz ../ebin -make

src_tests: ebin_tests src_src
	cd tests;erl -pz ../ebin -pz -make

src/sedate_parse.erl: priv/gherkin.peg
	${ERL} -noshell -s init stop -eval 'peg_gen:file("priv/gherkin.peg", [{output, "src"},{module, sedate_parse},{transform_module, sedate_ast}]).'

clean:
	rm -rf ebin/ ebin_tests/

tests: src_tests
	${ERL} -pz ebin -pz ebin_tests -noshell -s init stop -eval 'test_suite:test()'