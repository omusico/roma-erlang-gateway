include ../support/include.mk

all: $(EBIN_FILES) test

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)

test:
	erl -noshell -pa ../ebin -eval 'eunit:test("../ebin",[verbose])' -s init stop
