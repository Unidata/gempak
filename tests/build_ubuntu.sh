#!/bin/sh -xe

ls -l /home

# Required packages
apt-get update -y >& /dev/null
apt-get install build-essential gfortran git gcc g++ libx11-dev libxt-dev libxext-dev libxft-dev libxtst-dev flex byacc python-dev libmotif-dev libxml2-dev libxslt-dev libz-dev autoconf  -y >& /dev/null

# Package GEMPAK source from HEAD
pushd /gempak
package_version=`grep "define version" rpm/Installer.gempak/docker.spec | grep -v version_core| awk '{print $3}'`
git archive --format=tar --prefix=gempak-${package_version}/ HEAD  | gzip >/tmp/gempak-${package_version}.tar.gz

# Prepare the environment
mkdir -p /tmp/gempak-${package_version}/

cp -r /gempak/rpm/DEBIAN /tmp/gempak-${package_version}/

mkdir -p /home/gempak/
tar -xvzf /tmp/gempak-${package_version}.tar.gz -C /home/gempak >& /dev/null
pushd /home/gempak
mv gempak-${package_version} GEMPAK7
cd GEMPAK7
. Gemenviron.profile

pushd config

rm -rf Makeinc.linux64_gfortran
ln -s Makeinc.linux64_gfortran_ubuntu Makeinc.linux64_gfortran
popd

make extlibs >& /dev/null
make gempak >& /dev/null
make install >& /dev/null
make programs_gf >& /dev/null
make programs_nc >& /dev/null
make clean >& /dev/null

ls -la $OS_BIN|wc -l

mkdir -p /tmp/gempak-${package_version}/home
cp -r /home/gempak /tmp/gempak-${package_version}/home/

pushd /tmp

# Build the RPM
dpkg-deb --build gempak-${package_version}

dpkg -i gempak-${package_version}.deb

