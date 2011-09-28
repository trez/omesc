ERL     	= erl
EBIN			= ebin
LIBDIR    = $(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
VERSION   = $(shell cat VERSION | tr -d '\n')
CFLAGS    = -pa $(EBIN) -pa $(wildcard deps/*/ebin)
CC        = $(ERL) $(CFLAGS)
APP_NAME  = omesc

all: ebin compile
crun: all run

compile:
	./rebar compile

edoc:
	@echo Generating $(APP) documentation from srcs
	./rebar doc

ebin:
	@mkdir ebin

clean:
	./rebar clean

run:
	erl -pa ebin -sname omesc -eval 'io:format("~n"), application:start(omesc)'
