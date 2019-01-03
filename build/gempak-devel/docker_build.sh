#!/bin/bash
dir="$( cd "$(dirname "$0")" ; pwd -P )"
release=$(cat $dir/../../gempak/source/gemlib/ss/ssvers.f |grep "Version 7" | cut -d "'" -f2| cut -d " " -f2)

if [ -z "$1" ]; then
  echo "supply type (centos6, centos7, fedora, ubuntu)"
  exit
fi
os_version=$1

existing=$(sudo docker images |grep gempak-devel | grep $1 | awk '{ print $3 }')
if [ ! -z "$existing" ]; then
   sudo docker rmi $existing
fi
pushd $dir
sudo docker build --no-cache=true -t unidata/gempak-devel:${os_version} -f Dockerfile.${os_version} .
dockerID=$(sudo docker images | grep gempak-devel | grep latest | awk '{print $3}' | head -1 )
sudo docker tag $dockerID unidata/gempak-devel:${os_version} 
sudo docker rmi unidata/gempak-devel:latest
#sudo docker push unidata/gempak-devel
