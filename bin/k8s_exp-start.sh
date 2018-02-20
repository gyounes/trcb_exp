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
  PULL_IMAGE=IfNotPresent

fi

# start redis
"${DIR}"/k8s_redis-deploy.sh

# start dashboard
#"${DIR}"/trcb_exp-dash-deploy.sh

# trcb_exp configuration
# MODE_=(base dots)
# NODE_NUMBER_=(3 4 5)
# NODE_EVENT_NUMBER_=(10 20)
# DEFAULT_EVENT_INTERVAL_=(100 1000)
MODE_=(ping)
NODE_NUMBER_=(2)
NODE_EVENT_NUMBER_=(1000)
DEFAULT_EVENT_INTERVAL_=(1000)
KEEP_ALIVE=false

# shellcheck disable=SC2034
for REP in $(seq 1 $REPS)
do
  for MODE in "${MODE_[@]}"
  do
    for NODE_NUMBER in "${NODE_NUMBER_[@]}"
    do
      for NODE_EVENT_NUMBER in "${NODE_EVENT_NUMBER_[@]}"
      do
        for DEFAULT_EVENT_INTERVAL in "${DEFAULT_EVENT_INTERVAL_[@]}"
        do
          BRANCH=${BRANCH} \
          IMAGE=${IMAGE} \
          PULL_IMAGE=${PULL_IMAGE} \
          MODE=${MODE} \
          NODE_NUMBER=${NODE_NUMBER} \
          NODE_EVENT_NUMBER=${NODE_EVENT_NUMBER} \
          DEFAULT_EVENT_INTERVAL=${DEFAULT_EVENT_INTERVAL} \
          KEEP_ALIVE=${KEEP_ALIVE} "${DIR}"/k8s_exp-deploy.sh
        done
      done
    done
  done
done

#"${DIR}"/start-redis-sync.sh

#"${DIR}"/g-cluster.sh stop