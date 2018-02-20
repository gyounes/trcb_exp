#!/bin/sh

IP=$1
COUNTER=0

echo "[\c" > $2

while [ $COUNTER -le $3 ]
do
	AVERAGE_LATENCY=$(ping -c 1 -q $IP | tail -n 1 | cut -d/ -f5)
  	# echo "{${COUNTER},${AVERAGE_LATENCY}},\c" >> data$2
  	echo "${AVERAGE_LATENCY},\c" >> $2
  	COUNTER=$((COUNTER+1))
	sleep 1
done

echo "$(sed 's/.$//' $2)\c" > $2

echo "]\c" >> $2