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

ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$StoreName".emulab.net << EOF
cd trcb_exp
git pull
redis-server
EOF &&

echo -e "start store $StoreName done ${GREEN}successfully${NC}" &&

###

ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$SyncName".emulab.net << EOF
~/trcb_exp/bin/emu_exp-start.sh "$SyncIP" true
EOF &&

echo -e "start sync $SyncName done ${GREEN}successfully${NC}" &&

###

NodeNames=( $Names  )
NodeIPs=( $IPs  )

for index in ${!NodeNames[@]};
do
  echo "$index" &&
  echo "${NodeNames[index]}" &&
  echo "${NodeIPs[index]}" &&
    ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"${NodeNames[index]}".emulab.net << EOF
~/trcb_exp/bin/emu_exp-start.sh "${NodeIPs[index]}" false
EOF &&
    echo -e "start replica ${NodeNames[index]} done ${GREEN}successfully${NC}"
done


echo -e "${GREEN}init.sh DONE${NC}"
