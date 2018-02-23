RED='\033[0;31m'
NC='\033[0m' # No Color
GREEN='\033[0;32m'

StoreName=$(cat nodesNames | grep store | awk '{print $4}') &&
echo -e "Store Name done ${GREEN}successfully${NC}" &&

SyncName=$(cat nodesNames | grep sync | awk '{print $4}') &&
echo -e "Sync Name done ${GREEN}successfully${NC}" &&

ReplicasNames=$(cat nodesNames | grep replica | awk '{print $4}' | tr '\n' ' ') &&
echo -e "Replicas Names done ${GREEN}successfully${NC}" &&

###

# ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$StoreName".emulab.net 'cd trcb_exp; git pull; screen -S store -d -m redis-server' &&
ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$StoreName".emulab.net 'cd trcb_exp; git pull ; nohup redis-server > /dev/null 2>&1 &' &&
echo -e "start store $StoreName done ${GREEN}successfully${NC}" &&

###

# ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$SyncName".emulab.net "cd trcb_exp; SyncIP=\$(ifconfig | grep addr:10.1.1. | awk '{print \$2}' | grep -Eo '[0-9\.]+'); echo \$SyncIP; screen -S sync -d -m ~/trcb_exp/bin/emu_exp-start.sh \$SyncIP true" &&
ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$SyncName".emulab.net "cd trcb_exp; SyncIP=\$(ifconfig | grep addr:10.1.1. | awk '{print \$2}' | grep -Eo '[0-9\.]+'); echo \$SyncIP; ~/trcb_exp/bin/emu_exp-start.sh \$SyncIP true > /dev/null 2>&1 &" &&
echo -e "start sync $SyncName done ${GREEN}successfully${NC}" &&

sleep 15

###

NodeNames=( $ReplicasNames )

for Node in ${NodeNames[@]};
do
  # ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$Node".emulab.net "cd trcb_exp; NodeIP=\$(ifconfig | grep addr:10.1.1. | awk '{print \$2}' | grep -Eo '[0-9\.]+'); echo \$NodeIP; screen -S \$NodeIP -d -m ~/trcb_exp/bin/emu_exp-start.sh \$NodeIP false" &&
  ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$Node".emulab.net "cd trcb_exp; NodeIP=\$(ifconfig | grep addr:10.1.1. | awk '{print \$2}' | grep -Eo '[0-9\.]+'); echo \$NodeIP; ~/trcb_exp/bin/emu_exp-start.sh \$NodeIP false > /dev/null 2>&1 &" &&
  echo -e "start replica $Node done ${GREEN}successfully${NC}"
  sleep 15
done

echo -e "${GREEN}init.sh DONE${NC}"
