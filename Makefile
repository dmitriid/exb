ERL          ?= erl
ERLC          = erlc
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := exb

all: exmpp erl ebin/$(APP).app 

erl:
	@$(ERL) -pa $(EBIN_DIRS) -pa ebin -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

exmpp:
	@echo 'skipping exmpp..'
#	(cd deps/exmpp; make)

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean: 
	@echo "removing:"
#	(cd deps/exmpp; make clean)
	@rm -fv ebin/*.beam ebin/*.app
	@rm -fv priv/log/*

ebin/$(APP).app:
	@cp -v src/$(APP).app $@

