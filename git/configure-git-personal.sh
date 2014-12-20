#! /bin/sh
set -e
cd "$(dirname "$0")"
. ./configure-git-common.sh
git config --global user.email "lassi@lassikortela.net"
