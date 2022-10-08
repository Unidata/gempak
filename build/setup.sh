#!/bin/sh -xe
# ./setup.sh centos 7
# ./setup.sh centos stream8
# ./setup.sh centos stream9
# ./setup.sh fedora latest
# ./setup.sh ubuntu latest

os_type=$1
os_version=$2
TAG=${os_type}${os_version}

 # Run builds in Container

if [ "$os_type" = "dev" ]; then
	
	# sudo docker run --entrypoint=/bin/bash --privileged -d -ti -e "container=docker" -v /awips2/repo/${dn}:/awips2/repo/${dn} $imgname:$imgvers-$os_version
        sudo docker run -v `pwd`:/gempak:rw unidata/gempak-devel:centos6 /bin/bash -c "bash -xe /gempak/build/build_dev.sh"

elif [ "$os_type" = "centos" ]; then

        sudo docker run --privileged -d -ti -e "container=docker" -v `pwd`:/gempak:rw quay.io/centos/centos:$os_version /bin/bash
        DOCKER_CONTAINER_ID=$(sudo docker ps | grep ${os_version} | awk '{print $1}' | head -1 )
        sudo docker logs $DOCKER_CONTAINER_ID
        sudo docker exec --tty $DOCKER_CONTAINER_ID /bin/bash -xec "bash -xe /gempak/build/build_${os_type}.sh ${os_version}";
        sudo docker ps -a
        sudo docker stop $DOCKER_CONTAINER_ID
        sudo docker rm -v $DOCKER_CONTAINER_ID

elif [ "$os_type" = "ubuntu" ]; then

    sudo docker run --rm=true -v `pwd`:/gempak:rw unidata/gempak-devel:$os_type /bin/bash -c "bash -xe /gempak/build/build_${os_type}.sh "

elif [ "$os_type" = "fedora" ]; then

    sudo docker run --rm=true -v `pwd`:/gempak:rw unidata/gempak-devel:$os_type /bin/bash -c "bash -xe /gempak/build/build_${os_type}.sh"

fi
