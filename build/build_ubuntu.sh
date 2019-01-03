#!/bin/sh -xe
ls -l /home

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
export PYINC="-I/usr/include/python2.7"
export PYLIB="-lpython2.7"
export WITHPY="-DWITHPYTHON"
export PYDEP="-lpthread -lutil -ldl"
export LDFLAGS="-L/usr/lib -L$OS_LIB -s"

pushd config
rm -rf Makeinc.linux64_gfortran
ln -s Makeinc.linux64_gfortran_ubuntu Makeinc.linux64_gfortran
ls -latr
popd

# Build GEMPAK
make everything

#gemlog="/gempak/build/dist/make.gempak.log"
#cd extlibs/HDF5
#make all
#ls -latr $OS_LIB
#ls -la $OS_INC
#cd ../netCDF
#make all

#make extlibs 2>&1 | tee -a /gempak/build/dist/make.extlibs.log
#make gempak 2>&1 | tee -a $gemlog
#make install >& /dev/null
#make programs_gf >& /dev/null
#make programs_nc >& /dev/null
#make clean >& /dev/null
#grep -i error $gemlog

# Cleanup
rm -rf .gitignore .travis.yml

# Build the deb package
mkdir -p /tmp/gempak-${package_version}/home
cp -r /home/gempak /tmp/gempak-${package_version}/home/
pushd /tmp
dpkg-deb --nocheck --debug --verbose --build gempak-${package_version}
cp gempak-${package_version}.deb /gempak/build/dist/

# Confirm install with dependencies
apt-get update -y
dpkg -i gempak-${package_version}.deb
apt-get -f install
