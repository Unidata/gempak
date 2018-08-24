#!/bin/sh -xe
# ./setup.sh centos 7
# ./setup.sh centos 6
# ./setup.sh fedora latest

os_type=$1
os_version=$2
ORG=unidata
IMAGE=gempak-devel
TAG=${os_type}${os_version}

 # Run builds in Container
if [ "$os_type" = "centos" ]; then

    if [ "$os_version" = "container" ]; then

        sudo docker build -t unidata/gempak -f build/docker/Dockerfile.gempak build/docker

    elif [ "$os_version" = "6" ]; then

        sudo docker run --rm=true -v `pwd`:/gempak:rw unidata/$IMAGE:$TAG /bin/bash -c "bash -xe /gempak/build/extlibs/build_extlibs.sh"

    elif [ "$os_version" = "7" ]; then

        sudo docker run --privileged -d -ti -e "container=docker" -v `pwd`:/gempak:rw unidata/$IMAGE:$TAG /usr/sbin/init
        DOCKER_CONTAINER_ID=$(sudo docker ps | grep ${os_version} | awk '{print $1}' | head -1 )
        sudo docker logs $DOCKER_CONTAINER_ID
        sudo docker exec -ti $DOCKER_CONTAINER_ID /bin/bash -xec "bash -xe /gempak/build/extlibs/build_extlibs.sh";
        sudo docker ps -a
        sudo docker stop $DOCKER_CONTAINER_ID
        sudo docker rm -v $DOCKER_CONTAINER_ID

    fi

elif [ "$os_type" = "fedora" ]; then

    sudo docker run --rm=true -v `pwd`:/gempak:rw unidata/$IMAGE:fedora /bin/bash -c "bash -xe /gempak/build/extlibs/build_extlibs.sh"

fi
