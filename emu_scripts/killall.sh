RED='\033[0;31m'
NC='\033[0m' # No Color
GREEN='\033[0;32m'

SyncIP=$(cat nodesIPs | grep sync | awk '{print $3}') &&
echo -e "Sync IP done ${GREEN}successfully${NC}" &&

ReplicasIPs=$(cat nodesIPs | grep replica | awk '{print $3}' | tr '\n' ' ') &&
echo -e "Replicas IPs done ${GREEN}successfully${NC}" &&

###

StoreName=$(cat nodesNames | grep store | awk '{print $4}') &&
echo -e "Store Name done ${GREEN}successfully${NC}" &&

SyncName=$(cat nodesNames | grep sync | awk '{print $4}') &&
echo -e "Sync Name done ${GREEN}successfully${NC}" &&

ReplicasNames=$(cat nodesNames | grep replica | awk '{print $4}' | tr '\n' ' ') &&
echo -e "Replicas Names done ${GREEN}successfully${NC}" &&

###

ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$SyncName".emulab.net 'rm -rf ~/trcb_exp/_build/; pkill -9 beam.smp' &&

echo -e "kill sync $SyncName done ${GREEN}successfully${NC}" &&

###

NodeNames=( $Names  )
NodeIPs=( $IPs  )

for index in ${!NodeNames[@]};
do
  echo "$index" &&
  echo "${NodeNames[index]}" &&
  echo "${NodeIPs[index]}" &&
    ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"${NodeNames[index]}".emulab.net 'rm -rf ~/trcb_exp/_build/; pkill -9 beam.smp' &&
    echo -e "kill replica ${NodeNames[index]} done ${GREEN}successfully${NC}"
done

###

ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$StoreName".emulab.net "pid=\$(ps -aux | grep redis-server | grep -v grep | awk '{print \$2}'); kill -9 \$pid"

echo -e "kill store $StoreName done ${GREEN}successfully${NC}" &&

###

echo -e "${GREEN}killall.sh DONE${NC}"
