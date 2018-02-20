#!/usr/bin/env bash

ARG1=$1
ARG2=$2

echo $ARG1
echo $ARG2

./trcb_exp/rebar3 release -d

IP=$ARG1 \
SYNCHRONIZER=$ARG2 \
MODE=ping \
NODE_NUMBER=2 \
NODE_EVENT_NUMBER=1000 \
DEFAULT_EVENT_INTERVAL=1000 \
ORCHESTRATION=emulab \
METRICS_STORE=redis \
KEEP_ALIVE=false \
~/trcb_exp/_build/default/rel/trcb_exp/bin/env