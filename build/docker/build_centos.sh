#!/bin/bash
. ~/GEMPAK7/Gemenviron.profile
version=$(cat $GEMPAK/source/gemlib/ss/ssvers.f |grep "Version 7" | cut -d "'" -f2| cut -d " " -f2)
pushd $NAWIPS/build/docker
sudo docker build -t unidata/gempak -f Dockerfile.centos .
dockerID=$(sudo docker images | grep gempak | grep latest | awk '{print $3}' | head -1 )
sudo docker tag -f $dockerID unidata/gempak:${version}
sudo docker push unidata/gempak
