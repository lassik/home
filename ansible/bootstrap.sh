#!/bin/sh -
set -eu
host="$1"
ssh "root@$host" apt-get install -y python
ansible-playbook bootstrap1.yml -i "$host", -k
ansible-playbook bootstrap2.yml -i "$host",
