ERL=erl
ERLC=erlc
ERLC_OPT=+debu_info -W -o ebin

all: src_src

ebin:
	mkdir ebin

src_src: src/sedate_parse.erl ebin
	cd src;erl -pz ../ebin -make

src/sedate_parse.erl: priv/gherkin.peg
	${ERL} -noshell -s init stop -eval 'peg_gen:file("priv/gherkin.peg", [{output, "src"},{module, sedate_parse},{transform_module, sedate_ast}]).'

clean:
	rm -rf ebin/