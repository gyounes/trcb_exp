#!/usr/bin/env bash

ARG1=$1
ARG2=$2

if [ $2=true ]; then
   ./rebar3 release -d
else
	cp -r ./_build ./_build$1
fi


IP=$ARG1 \
SYNCHRONIZER=$ARG2 \
MODE=ping \
NODE_NUMBER=2 \
NODE_EVENT_NUMBER=1000 \
DEFAULT_EVENT_INTERVAL=1000 \
ORCHESTRATION=emulab \
METRICS_STORE=redis \
KEEP_ALIVE=false \
./_build$1/default/rel/trcb_exp/bin/env