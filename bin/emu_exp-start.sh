#!/usr/bin/env bash

IP=$1 \
SYNCHRONIZER=$2 \
MODE=ping \
NODE_NUMBER=2 \
NODE_EVENT_NUMBER=50 \
DEFAULT_EVENT_INTERVAL=1000 \
LATENCY=0 \
ORCHESTRATION=emulab \
METRICS_STORE=redis \
KEEP_ALIVE=false \
sudo /trcb/_build/default/rel/trcb_exp/bin/env