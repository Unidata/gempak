#!/bin/bash -xe
. ~/GEMPAK7/Gemenviron.profile
pushd $NAWIPS/build/docker
version=$(cat $GEMPAK/source/gemlib/ss/ssvers.f |grep "Version 7" | cut -d "'" -f2| cut -d " " -f2)
sudo docker build -t unidata/gempak -f Dockerfile.ubuntu .
dockerID=$(sudo docker images | grep gempak | grep latest | awk '{print $3}' | head -1 )
sudo docker tag -f $dockerID unidata/gempak:ubuntu-${version}
sudo docker push unidata/gempak
