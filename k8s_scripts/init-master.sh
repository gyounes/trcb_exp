RED='\033[0;31m'
NC='\033[0m' # No Color
GREEN='\033[0;32m'

sudo swapoff -a
echo -e "sudo swapoff -a ${GREEN}successfull${NC}"

sudo groupadd docker
echo -e "sudo groupadd docker ${GREEN}successfull${NC}"

sudo systemctl start docker.service
echo -e "sudo systemctl start docker.service ${GREEN}successfull${NC}"

APISERV=$(ifconfig | grep -Eb1 "enp6s7" | grep -Eo "inet addr:[0-9\.]+" | awk -F : '{print $2}') &&
echo -e "APISERVER ADVERTISE ADDRESS is ${APISERV}" && 

# sudo kubeadm init --pod-network-cidr=192.168.0.0/16 --apiserver-advertise-address=${APISERV} --kubernetes-version stable-1.8 2>&1 | tee tmp &&
sudo kubeadm init --pod-network-cidr=10.244.0.0/16 --apiserver-advertise-address=${APISERV} --kubernetes-version stable-1.8 2>&1 | tee tmp &&

#sudo kubeadm init --apiserver-advertise-address=${APISERV} --kubernetes-version stable-1.8 2>&1 | tee tmp &&

KUBEADMINIT=$(tail -2 tmp | grep join) &&
echo "${KUBEADMINIT}" > ~/scripts/kubeadmjoin &&
echo -e "sudo kubeadm init ${GREEN}successfull${NC}" &&

mkdir -p $HOME/.kube && 
echo -e "mkdir -p $HOME/.kube ${GREEN}successfull${NC}" &&

yes | sudo cp -i /etc/kubernetes/admin.conf $HOME/.kube/config && 
echo -e "sudo cp -i /etc/kubernetes/admin.conf $HOME/.kube/config  ${GREEN}successfull${NC}" &&

sudo chown `id -u`:`id -g` $HOME/.kube/config &&
echo -e "sudo chown `id -u`:`id -g` $HOME/.kube/config ${GREEN}successfull${NC}" &&

sudo kubectl apply -f https://raw.githubusercontent.com/coreos/flannel/v0.9.1/Documentation/kube-flannel.yml &&
# sudo kubectl apply -f https://raw.githubusercontent.com/coreos/flannel/master/Documentation/kube-flannel.yml &&
echo -e "sudo kubectl apply -f https://raw.githubusercontent.com/coreos/flannel/master/Documentation/kube-flannel.yml ${GREEN}successfull${NC}" &&

sudo kubectl create clusterrolebinding add-on-cluster-admin --clusterrole=cluster-admin --serviceaccount=default:default &&
echo -e "sudo kubectl create clusterrolebinding add-on-cluster-admin --clusterrole=cluster-admin --serviceaccount=default:default ${GREEN}successfull${NC}" &&

arg1=$1; shift
NODES=( "$@" )
#array=( "$@" )
#last_idx=$(( ${#array[@]} - 1 ))
#NODES=${array[$last_idx]}
#unset array[$last_idx]

for NODE in ${NODES[@]};
do
  echo "$NODE" &&
    ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" gyounes@"$NODE".emulab.net 'bash -s' < $arg1 &&
    echo -e "ssh... ${GREEN}successfull${NC}"
done 
echo -e "${GREEN}init-master.sh DONE${NC}"
