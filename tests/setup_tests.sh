#!/bin/sh -xe

# This script starts docker and systemd (if el7)

# Version of CentOS/RHEL
os_type=$1
el_version=$2

 # Run tests in Container
if [[ "$os_type" = "centos" ]]; then
    if [[ "$el_version" = "6" ]]; then

        sudo docker run --rm=true -v `pwd`:/gempak:rw centos:centos${el_version} /bin/bash -c "bash -xe /gempak/tests/build_rhel.sh ${os_type} ${el_version}"

    elif [[ "$el_version" = "7" ]]; then

        docker run --privileged -d -ti -e "container=docker"  -v /sys/fs/cgroup:/sys/fs/cgroup -v `pwd`:/gempak:rw  centos:centos${el_version}   /usr/sbin/init
        DOCKER_CONTAINER_ID=$(docker ps | grep centos | awk '{print $1}')
        docker logs $DOCKER_CONTAINER_ID
        docker exec -ti $DOCKER_CONTAINER_ID /bin/bash -xec "bash -xe /gempak/tests/build_rhel.sh ${os_type} ${el_version}";
        docker ps -a
        docker stop $DOCKER_CONTAINER_ID
        docker rm -v $DOCKER_CONTAINER_ID

    fi

elif [[ "$os_type" = "ubuntu" ]]; then

    sudo docker run --rm=true -v `pwd`:/gempak:rw ubuntu:latest /bin/bash -c "bash -xe /gempak/tests/build_ubuntu.sh "

fi
