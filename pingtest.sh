#!/bin/sh

IP=$1
COUNTER=0

echo "index | Average" > data$2

while [ $COUNTER -le 10000 ]
do
	AVERAGE_LATENCY=$(ping -c 1 -q $IP | tail -n 1 | cut -d/ -f5)
  	echo "${COUNTER}  |  ${AVERAGE_LATENCY} \n" >> data
  	COUNTER=$((COUNTER+1))
	sleep 1
done