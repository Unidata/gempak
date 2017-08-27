#!/bin/sh -xe

ls -l /home

# Required packages
apt-get update -y >& /dev/null
apt-get install build-essential gfortran git gcc g++ libx11-dev libxt-dev libxext-dev libxft-dev libxtst-dev flex byacc python-dev libmotif-dev libxml2-dev libxslt-dev libz-dev autoconf -y

# Package GEMPAK source from HEAD
pushd /gempak
package_version=`grep "define version" build/Installer.gempak/gempak.spec | grep -v version_core| awk '{print $3}'`
git archive --format=tar --prefix=gempak-${package_version}/ HEAD  | gzip >/tmp/gempak-${package_version}.tar.gz

# Prepare the environment
mkdir -p /tmp/gempak-${package_version}/

cp -r /gempak/build/DEBIAN /tmp/gempak-${package_version}/

mkdir -p /home/gempak/
tar -xvzf /tmp/gempak-${package_version}.tar.gz -C /home/gempak >& /dev/null
pushd /home/gempak
mv gempak-${package_version} GEMPAK7
cd GEMPAK7
. Gemenviron.profile
. source_python.sh

pushd config

rm -rf Makeinc.linux64_gfortran
ln -s Makeinc.linux64_gfortran_ubuntu Makeinc.linux64_gfortran
popd

make extlibs
make gempak
make install
make programs_gf
make programs_nc
make clean >& /dev/null

rm -rf extlibs config .gitignore .travis.yml build


ls -la $OS_BIN|wc -l

mkdir -p /tmp/gempak-${package_version}/home
cp -r /home/gempak /tmp/gempak-${package_version}/home/

pushd /tmp

# Build the RPM
dpkg-deb --nocheck --debug --verbose --build gempak-${package_version}
cp gempak-${package_version}.deb /gempak/build/dist/

# Install with dependencies
apt-get update -y
dpkg -i gempak-${package_version}.deb
apt-get -f install
