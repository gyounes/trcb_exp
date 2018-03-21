#!/usr/bin/env bash

REPS=1
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOCKER_USER=gyounes
IMAGE=${DOCKER_USER}/trcb_exp-copy

#"${DIR}"/g-cluster.sh start

if [ "$1" == "build" ]; then
  # build and push
  DOCKERFILE=${DIR}/../Dockerfiles/trcb_exp-copy

  IMAGE=${IMAGE} \
    DOCKERFILE=${DOCKERFILE} "${DIR}"/image.sh

  # use the new image
  PULL_IMAGE=Always

elif [ "$1" == "local" ]; then
  # build locally
  IMAGE=${DOCKER_USER}/trcb_exp-copy
  DOCKERFILE=${DIR}/../Dockerfiles/trcb_exp-copy

  # eval $(minikube docker-env)

  docker build \
         --no-cache \
         -t "${IMAGE}" -f "${DOCKERFILE}" .

  # use the new image
  PULL_IMAGE=Never

else
  # use the latest image
  PULL_IMAGE=Always

fi

# start redis
"${DIR}"/k8s_redis-deploy.sh

# start dashboard
#"${DIR}"/trcb_exp-dash-deploy.sh

# trcb_exp configuration
MODE_=(base dots)
NODE_NUMBER_=(32)
DEFAULT_EVENT_INTERVAL_=(5)
# DEFAULT_MSG_NUMBER=50
DROP_PERCENT_=(5)
NODE_EVENT_NUMBER=1000
LATENCY_=(10)
CPU=7

# MODE_=(base dots ping)
# NODE_NUMBER_=(5 10)
# NODE_EVENT_NUMBER_=(100 1000)
# DEFAULT_EVENT_INTERVAL_=(10 100)
# LATENCY_=(0 20)
# CPU=7

# shellcheck disable=SC2034
for REP in $(seq 1 $REPS)
do
  for MODE in "${MODE_[@]}"
  do
    for NODE_NUMBER in "${NODE_NUMBER_[@]}"
    do
      for DEFAULT_EVENT_INTERVAL in "${DEFAULT_EVENT_INTERVAL_[@]}"
      do
        for LATENCY in "${LATENCY_[@]}"
        do
          for DROP_PERCENT in "${DROP_PERCENT_[@]}"
          do
            BRANCH=${BRANCH} \
            IMAGE=${IMAGE} \
            PULL_IMAGE=${PULL_IMAGE} \
            MODE=${MODE} \
            NODE_NUMBER=${NODE_NUMBER} \
            DEFAULT_EVENT_INTERVAL=${DEFAULT_EVENT_INTERVAL} \
            NODE_EVENT_NUMBER=${NODE_EVENT_NUMBER} \
            DROP_PERCENT=${DROP_PERCENT} \
            LATENCY=${LATENCY} \
            CPU=${CPU} "${DIR}"/k8s_exp-deploy.sh
          done
        done
      done
    done
  done
done
            # NODE_EVENT_NUMBER=${NODE_EVENT_NUMBER} \
            # NODE_EVENT_NUMBER=$((1000/${DEFAULT_EVENT_INTERVAL}*${DEFAULT_MSG_NUMBER})) \

#"${DIR}"/start-redis-sync.sh

#"${DIR}"/g-cluster.sh stop