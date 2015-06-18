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

read -p "Title for public keys: " KEYTITLE
GITHUBKEY=`cat ~/.ssh/id_github4096_rsa.pub`
BITBUCKETKEY=`cat ~/.ssh/id_bitbucket4096_rsa.pub`
http -a storax --json https://api.github.com/user/keys title="$KEYTITLE" key="$GITHUBKEY"
http -a dzuber --json https://bitbucket.org/api/1.0/users/dzuber/ssh-keys label="$KEYTITLE" key="$BITBUCKETKEY"
