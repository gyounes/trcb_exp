PACKAGE         ?= trcb_exp
VERSION         ?= $(shell git describe --tags)
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR            = $(shell pwd)/rebar3
MAKE						 = make

.PHONY: test

all: compile

##
## Compilation targets
##

compile:
	$(REBAR) compile

##
## Test targets
##

check: test xref dialyzer lint

test: ct eunit
	${REBAR} cover -v

lint: erl-lint #shell-lint docker-lint

erl-lint:
	${REBAR} as lint lint

shell-lint:
	ls -d bin/* | grep -v ".erl" | xargs shellcheck

docker-lint:
	for f in $$(ls -d Dockerfiles/*); do dockerlint $$f; done

eunit:
	${REBAR} eunit

ct: trcbtest

trcbtest:
	${REBAR} ct --suite=test/trcb_exp_SUITE

xref:
	${REBAR} xref skip_deps=true

dialyzer:
	${REBAR} dialyzer

cover: test
	open _build/test/cover/index.html

shell:
	${REBAR} shell --apps ${PACKAGE}

##
## Release targets
##

stage:
	${REBAR} release -d

##
## Experiments targets
##

logs:
	  tail -F priv/lager/*/log/*.log

run:
	  _build/default/rel/${PACKAGE}/bin/env

DIALYZER_APPS = kernel stdlib erts sasl eunit syntax_tools compiler crypto
