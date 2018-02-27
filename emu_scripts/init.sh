RED='\033[0;31m'
NC='\033[0m' # No Color
GREEN='\033[0;32m'

StoreName=$(cat nodesNames | grep store | awk '{print $4}') &&
echo -e "Store Name done ${GREEN}successfully${NC}" &&

SyncName=$(cat nodesNames | grep sync | awk '{print $4}') &&
echo -e "Sync Name done ${GREEN}successfully${NC}" &&

ReplicasNames=$(cat nodesNames | grep replica | awk '{print $4}' | tr '\n' ' ') &&
echo -e "Replicas Names done ${GREEN}successfully${NC}" &&

#################################

# ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$StoreName".emulab.net 'cd ~/trcb_exp; git pull;./rebar3 release -d; nohup redis-server > /dev/null 2>&1 &' &&
# ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$StoreName".emulab.net 'cd ~/trcb_exp ; git pull ; ./rebar3 release -d ; screen -S store -d -m redis-server' &&
ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$StoreName".emulab.net 'cd ~/trcb_exp ; git pull ; ./rebar3 release -d ; nohup redis-server > /dev/null 2>&1 &' &&
echo -e "start store $StoreName done ${GREEN}successfully${NC}" &&

#################################

ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$SyncName".emulab.net "sudo mkdir /trcb; sudo chmod 777 /trcb ; sudo cp -rf ~/trcb_exp/_build/ /trcb" &&
echo -e "start sync $SyncName done ${GREEN}successfully${NC}" &&

NodeNames=( $ReplicasNames )

for Node in ${NodeNames[@]};
do
  ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$Node".emulab.net "sudo mkdir /trcb ; sudo chmod 777 /trcb ; sudo cp -rf ~/trcb_exp/_build/ /trcb" &&
  echo -e "start replica $Node done ${GREEN}successfully${NC}"
done

sleep 7
#################################

# ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$SyncName".emulab.net "sudo pkill -9 beam.smp; lsof -i:6866; SyncIP=\$(ifconfig | grep addr:10.1.1. | awk '{print \$2}' | grep -Eo '[0-9\.]+'); echo \$SyncIP; nohup ~/trcb_exp/bin/emu_exp-start.sh \$SyncIP true & > \$SyncIP" &&
ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$SyncName".emulab.net "sudo pkill -9 beam.smp; lsof -i:6866; sudo mkdir /trcb /priv; sudo chmod 777 /trcb /priv; sudo cp -rf ~/trcb_exp/_build/ /trcb ; SyncIP=\$(ifconfig | grep addr:10.1.1. | awk '{print \$2}' | grep -Eo '[0-9\.]+'); echo \$SyncIP; screen -S sync -d -m  ~/trcb_exp/bin/emu_exp-start.sh \$SyncIP true" &&
echo -e "start sync $SyncName done ${GREEN}successfully${NC}" &&

sleep 5

for Node in ${NodeNames[@]};
do
  # ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$Node".emulab.net "sudo pkill -9 beam.smp; lsof -i:6866; NodeIP=\$(ifconfig | grep addr:10.1.1. | awk '{print \$2}' | grep -Eo '[0-9\.]+'); echo \$NodeIP; nohup ~/trcb_exp/bin/emu_exp-start.sh \$NodeIP false & > \$NodeIP" &&
  ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$Node".emulab.net "sudo pkill -9 beam.smp; lsof -i:6866; sudo mkdir /trcb ; sudo chmod 777 /trcb ; sudo cp -rf ~/trcb_exp/_build/ /trcb ; NodeIP=\$(ifconfig | grep addr:10.1.1. | awk '{print \$2}' | grep -Eo '[0-9\.]+'); echo \$NodeIP; screen -S \$NodeIP -d -m  ~/trcb_exp/bin/emu_exp-start.sh \$NodeIP false" &&
  echo -e "start replica $Node done ${GREEN}successfully${NC}"
  sleep 7
done

echo -e "${GREEN}init.sh DONE${NC}"

# tail -f ./*.log
