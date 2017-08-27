#!/bin/bash
set -e

if [ "$1" = 'runldm.sh' ]; then

    # don't chown var directory, it takes too long if there is lots of data
    cd ${HOME} && \
        chown -R ldm:ldm $(ls -A | awk '{if($1 != "var"){ print $1 }}') && \
        chown root ./bin/ldmd && chmod 4755 ./bin/ldmd && \
        chown root ./bin/hupsyslog && chmod 4755 ./bin/hupsyslog

    sync

    exec gosu ldm "$@"
fi

exec "$@"

