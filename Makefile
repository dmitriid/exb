ERL          ?= erl
ERLC          = erlc
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := exb
DOCPATH      := doc

all: exmpp erl ebin/$(APP).app 

erl:
	@$(ERL) -pa $(EBIN_DIRS) -pa ebin -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

exmpp:
	@echo 'skipping exmpp..'
#	(cd deps/exmpp; make)

docs:
	@erl -noshell -eval 'edoc:application(exb, ".", [{source_path, ["src", "priv"]}, {packages, false}]).'

clean: 
	@echo "removing:"
#	(cd deps/exmpp; make clean)
	@rm -fv ebin/*.beam ebin/*.app
	@rm -fv priv/log/*

ebin/$(APP).app:
	@cp -v src/$(APP).app $@

