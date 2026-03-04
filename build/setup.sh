#!/bin/sh -xe
# ./setup.sh almalinux 9
# ./setup.sh almalinux 10
# ./setup.sh fedora latest
# ./setup.sh ubuntu latest

os_type=$1
os_version=$2
TAG=${os_type}${os_version}

if [ -z "$os_type" ] || [ -z "$os_version" ]; then
    echo "Usage: $0 <dev|almalinux|ubuntu|fedora> <version>" >&2
    exit 2
fi

 # Run builds in Container

if [ "$os_type" = "dev" ]; then

    # sudo docker run --entrypoint=/bin/bash --privileged -d -ti -e "container=docker" -v /awips2/repo/${dn}:/awips2/repo/${dn} $imgname:$imgvers-$os_version
    sudo docker run -v `pwd`:/gempak:rw unidata/gempak-devel:centos6 /bin/bash -c "bash -xe /gempak/build/build_dev.sh"

elif [ "$os_type" = "almalinux" ]; then

    CONTAINER_IMAGE=quay.io/almalinuxorg/${os_version}-base
    BUILD_SCRIPT=build_almalinux.sh

    DOCKER_CONTAINER_ID=$(sudo docker run --privileged -d -ti -e "container=docker" -v `pwd`:/gempak:rw $CONTAINER_IMAGE /bin/bash)
        sudo docker logs $DOCKER_CONTAINER_ID
        sudo docker exec --tty $DOCKER_CONTAINER_ID /bin/bash -xec "bash -xe /gempak/build/${BUILD_SCRIPT} ${os_version}";
        sudo docker ps -a
        sudo docker stop $DOCKER_CONTAINER_ID
        sudo docker rm -v $DOCKER_CONTAINER_ID

elif [ "$os_type" = "ubuntu" ]; then

    sudo docker run --rm=true -v `pwd`:/gempak:rw ubuntu:${os_version} /bin/bash -c "bash -xe /gempak/build/build_${os_type}.sh "

elif [ "$os_type" = "fedora" ]; then

    sudo docker run --rm=true -v `pwd`:/gempak:rw unidata/gempak-devel:$os_type /bin/bash -c "bash -xe /gempak/build/build_${os_type}.sh"

else

    echo "Unsupported os_type '$os_type'. Expected one of: dev, almalinux, ubuntu, fedora." >&2
    exit 2

fi
