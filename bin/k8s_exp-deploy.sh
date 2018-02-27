#!/usr/bin/env bash

ENV_VARS=(
  IMAGE
  PULL_IMAGE
  MODE
  NODE_NUMBER
  NODE_EVENT_NUMBER
  DEFAULT_EVENT_INTERVAL
  LATENCY
  KEEP_ALIVE
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
echo "    LATENCY: ${LATENCY}"
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
apiVersion: extensions/v1beta1
kind: Deployment
metadata:
  name: "${SYNCRONIZER_NAME}"
spec:
  replicas: 1
  template:
    metadata:
      labels:
        timestamp: "${TIMESTAMP}"
        tag: synchronizer
    spec:
      containers:
      - name: "${SYNCRONIZER_NAME}"
        image: "${IMAGE}"
        imagePullPolicy: "${PULL_IMAGE}"
        resources:
          requests:
            cpu: 2
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
        - name: LATENCY
          value: "${LATENCY}"
        - name: SYNCHRONIZER
          value: "true"
---
apiVersion: extensions/v1beta1
kind: Deployment
metadata:
  name: "${EXP_NAME}"
spec:
  replicas: ${NODE_NUMBER}
  template:
    metadata:
# Enabling Unsafe Sysctls:
# - Docs: https://kubernetes.io/docs/concepts/cluster-administration/sysctl-cluster/#safe-vs-unsafe-sysctls
# - PR:   https://github.com/kubernetes/kubernetes/pull/26057/files?short_path=8dc23ab#diff-8dc23ab258695ee42154d4d1238c36ef
# - Help: http://www.ehowstuff.com/configure-linux-tcp-keepalive-setting/
#      annotations:
#        security.alpha.kubernetes.io/unsafe-sysctls: net.ipv4.tcp_keepalive_time=10,net.ipv4.tcp_keepalive_intvl=5,net.ipv4.tcp_keepalive_probes=1
      labels:
        timestamp: "${TIMESTAMP}"
        tag: exp
    spec:
      containers:
      - name: "${EXP_NAME}"
        image: "${IMAGE}"
        imagePullPolicy: "${PULL_IMAGE}"
        resources:
          requests:
            cpu: "${CPU}"
        securityContext:
          privileged: true
        env:
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
        - name: LATENCY
          value: "${LATENCY}"
        - name: KEEP_ALIVE
          value: "${KEEP_ALIVE}"
        - name: SYNCHRONIZER
          value: "false"
EOF

kubectl create -f "${FILE}"

while [ $(kubectl get pods -l timestamp=$TIMESTAMP 2>/dev/null | grep exp | wc -l) -gt 0 ]; do
    sleep 1
done

echo "Test done."
