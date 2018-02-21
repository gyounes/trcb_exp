#!/usr/bin/env bash

POD_NAME=$(kubectl get pods |
           grep redis |
           awk '{print $1}')

PORT=6379
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

METRICS_DIR=${DIR}/../priv/evaluation/metrics

echo "    POD_NAME: ${POD_NAME}"
echo "    PORT: ${PORT}"
echo "    DIR: ${DIR}"
echo "    METRICS_DIR: ${METRICS_DIR}"


kubectl port-forward "${POD_NAME}" ${PORT}:${PORT} & TUNNEL_PID=$!

echo "[$(date +%T)] Port forwarding starting..."
sleep 3

cd "${DIR}"/..
METRICS_DIR=${METRICS_DIR} "${DIR}"/redis-sync.erl

echo "[$(date +%T)] All files downloaded!"

kill ${TUNNEL_PID}

