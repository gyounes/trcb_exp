RED='\033[0;31m'
NC='\033[0m' # No Color
GREEN='\033[0;32m'

Main=$(cat nodesTable | grep main | awk '{print $4}') &&
echo -e "Main done ${GREEN}successfull${NC}" &&

Args=$(cat nodesTable | grep -v main | awk '{print $4}' | tr '\n' ' ') &&
echo -e "Args done ${GREEN}successfull${NC}" &&

scp -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" ./init-node.sh gyounes@"$Main".emulab.net:~/scripts/init-node.sh &&
echo -e "scp init-node.sh done ${GREEN}successfull${NC}" &&

ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$Main".emulab.net 'bash -s'< ./init-master.sh "~/scripts/init-node.sh" "${Args[@]}" &&

scp -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$Main".emulab.net:~/.kube/config ~/.kube/config

echo -e "ssh init-master.sh done ${GREEN}successfull${NC}"
