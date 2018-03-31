#!/usr/bin/env bash

ENV_VARS=(
  IMAGE
  PULL_IMAGE
  MODE
  NODE_NUMBER
  NODE_EVENT_NUMBER
  DEFAULT_EVENT_INTERVAL
  DROP_RATIO
  LATENCY
  CHECK_RESEND_INTERVAL
  RESEND_INTERVAL
  CPU
)

for ENV_VAR in "${ENV_VARS[@]}"
do
  if [ -z "${!ENV_VAR}" ]; then
    echo ">>> ${ENV_VAR} is not configured; please export it."
    exit 1
  fi
done

echo "[$(date +%T)] Configuration: "
echo "    IMAGE: ${IMAGE}"
echo "    PULL_IMAGE: ${PULL_IMAGE}"
echo "    MODE: ${MODE}"
echo "    NODE_NUMBER: ${NODE_NUMBER}"
echo "    NODE_EVENT_NUMBER: ${NODE_EVENT_NUMBER}"
echo "    DEFAULT_EVENT_INTERVAL: ${DEFAULT_EVENT_INTERVAL}"
echo "    DROP_RATIO: ${DROP_RATIO}"
echo "    LATENCY: ${LATENCY}"
echo "    CHECK_RESEND_INTERVAL: ${CHECK_RESEND_INTERVAL}"
echo "    RESEND_INTERVAL: ${RESEND_INTERVAL}"
echo "    CPU: ${CPU}"

# ENV SETUP:
# Kubernetes server and auth token
CONTEXT=$(kubectl config view |
          grep current |
          awk '{print $2}')
CLUSTER=$(kubectl config view |
            grep -Eb2 "name: ${CONTEXT}$" |
            grep "cluster:" |
            awk '{print $3}')
APISERVER=$(kubectl config view |
            grep -Eb1 "name: ${CLUSTER}$" |
            grep "server:" |
            grep -Eo "https://[0-9\.:]+")
TOKEN=$(kubectl describe secret |
        grep "token:" |
        awk '{print $2}')

ORCHESTRATION=kubernetes
METRICS_STORE=redis

# Evaluation timestamp: unix timestamp + random
R=$(echo $RANDOM + 10000 | bc)
TIMESTAMP=$(date +%s)${R}

# Port
PEER_PORT=6866

# DEPLOYMENT:
# Deployment names
SYNCRONIZER_NAME=synchronizer-${TIMESTAMP}
EXP_NAME=exp-${TIMESTAMP}

# YAML file
FILE=/tmp/${TIMESTAMP}.yaml

cat <<EOF > "${FILE}"
apiVersion: v1
kind: Pod
metadata:
  name: "${SYNCRONIZER_NAME}"
  labels:
    tag: synchronizer
    timestamp: "${TIMESTAMP}"
spec:
  restartPolicy: Never
  containers:
  - name: "${SYNCRONIZER_NAME}"
    image: "${IMAGE}"
    imagePullPolicy: "${PULL_IMAGE}"
    resources:
      requests:
        cpu: "${CPU}"
    env:
    - name: ORCHESTRATION
      value: "${ORCHESTRATION}"
    - name: METRICS_STORE
      value: "${METRICS_STORE}"
    - name: IP
      valueFrom:
        fieldRef:
          fieldPath: status.podIP
    - name: APISERVER
      value: "${APISERVER}"
    - name: TOKEN
      value: "${TOKEN}"
    - name: TIMESTAMP
      value: "${TIMESTAMP}"
    - name: MODE
      value: "${MODE}"
    - name: NODE_NUMBER
      value: "${NODE_NUMBER}"
    - name: NODE_EVENT_NUMBER
      value: "${NODE_EVENT_NUMBER}"
    - name: DEFAULT_EVENT_INTERVAL
      value: "${DEFAULT_EVENT_INTERVAL}"
    - name: DROP_RATIO
      value: "${DROP_RATIO}"
    - name: LATENCY
      value: "${LATENCY}"
    - name: CHECK_RESEND_INTERVAL
      value: "${CHECK_RESEND_INTERVAL}"
    - name: RESEND_INTERVAL
      value: "${RESEND_INTERVAL}"
    - name: SYNCHRONIZER
      value: "true"
EOF

# start the synchronizer
kubectl create -f "${FILE}"

for ID in $(seq 1 ${NODE_NUMBER}); do
  cat <<EOF > "${FILE}"
apiVersion: v1
kind: Pod
metadata:
  name: "${EXP_NAME}-${ID}"
  labels:
    tag: exp
    timestamp: "${TIMESTAMP}"
    id: "${ID}"
spec:
  restartPolicy: Never
  containers:
  - name: "${EXP_NAME}"
    image: "${IMAGE}"
    imagePullPolicy: "${PULL_IMAGE}"
    resources:
      requests:
        cpu: "${CPU}"
    env:
    - name: ID
      value: "${ID}"
    - name: ORCHESTRATION
      value: "${ORCHESTRATION}"
    - name: METRICS_STORE
      value: "${METRICS_STORE}"
    - name: IP
      valueFrom:
        fieldRef:
          fieldPath: status.podIP
    - name: PEER_PORT
      value: "${PEER_PORT}"
    - name: APISERVER
      value: "${APISERVER}"
    - name: TOKEN
      value: "${TOKEN}"
    - name: TIMESTAMP
      value: "${TIMESTAMP}"
    - name: MODE
      value: "${MODE}"
    - name: NODE_NUMBER
      value: "${NODE_NUMBER}"
    - name: NODE_EVENT_NUMBER
      value: "${NODE_EVENT_NUMBER}"
    - name: DEFAULT_EVENT_INTERVAL
      value: "${DEFAULT_EVENT_INTERVAL}"
    - name: DROP_RATIO
      value: "${DROP_RATIO}"
    - name: LATENCY
      value: "${LATENCY}"
    - name: CHECK_RESEND_INTERVAL
      value: "${CHECK_RESEND_INTERVAL}"
    - name: RESEND_INTERVAL
      value: "${RESEND_INTERVAL}"
    - name: SYNCHRONIZER
      value: "false"
EOF
  ## add loop to test connection to kubectl and keep waiting in case connection is lost
  kubectl create -f "${FILE}"
done

# sleep 5
# ~/Documents/Git/kubetail/kubetail $TIMESTAMP > $TIMESTAMP &

while [ $(kubectl get pods -l timestamp=$TIMESTAMP 2>/dev/null | grep exp | wc -l) -gt 0 ]; do
    sleep 1
done

echo "Test done."
