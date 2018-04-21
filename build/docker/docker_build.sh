#!/bin/bash
. ~/GEMPAK7/Gemenviron.profile
version=7.4.2
pushd $NAWIPS/build/docker
sudo docker build -t unidata/gempak -f Dockerfile.gempak .
dockerID=$(sudo docker images | grep gempak | grep latest | awk '{print $3}' | head -1 )
sudo docker tag $dockerID unidata/gempak:${version}
sudo docker push unidata/gempak
