#!/bin/bash

# limit max number of open files to force pqact to close open file descriptors.
ulimit -n 1024
set -e
set -x
export PATH=/home/ldm/bin:$PATH

# Set hostname as container ip address since docker doesn't provide fully-qualified domain names
export IP=`hostname -i`
regutil -s $IP /hostname

trap "echo TRAPed signal" HUP INT QUIT KILL TERM

/usr/sbin/crond

ldmadmin clean
ldmadmin delqueue
ldmadmin mkqueue
ldmadmin start

# never exit
while true; do sleep 10000; done

ldmadmin watch

sleep 10

echo ""
echo ""
echo "Hit [RETURN] to exit"
read
