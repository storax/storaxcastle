#!/bin/bash

cd ~/.ssh

declare -a hosts=(
    github
    bitbucket
    hlrs
)

for host in "${hosts[@]}"
do
    echo "Create key pair for host: ${host}"
    ssh-keygen -t rsa -b 4096 -C "zuber.david@gmx.de" -f "id_${host}4096_rsa"
done
