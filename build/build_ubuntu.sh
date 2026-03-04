#!/bin/sh -xe
ls -l /home

export DEBIAN_FRONTEND=noninteractive
apt-get update -y
apt-get install -y --no-install-recommends \
	git gcc gfortran build-essential \
	libx11-dev libxt-dev libxext-dev libxft-dev libxtst-dev \
	flex byacc libmotif-common libmotif-dev libxpm4 libxpm-dev \
	dpkg-dev ca-certificates

# Package GEMPAK source from HEAD
pushd /gempak
package_version=`grep "define version" build/Installer.gempak/gempak.spec | grep -v version_core| awk '{print $3}'`
git config --global --add safe.directory /gempak
git archive --format=tar --prefix=gempak-${package_version}/ -o /tmp/gempak-${package_version}.tar HEAD
gzip -f /tmp/gempak-${package_version}.tar

# Prepare the environment
mkdir -p /tmp/gempak-${package_version}/

cp -r /gempak/build/DEBIAN /tmp/gempak-${package_version}/

mkdir -p /home/gempak/
srcdir=$(tar -tzf /tmp/gempak-${package_version}.tar.gz | awk -F/ 'NF>1{print $1; exit}')
if [ -z "$srcdir" ]; then
	echo "Failed to determine top-level source directory from /tmp/gempak-${package_version}.tar.gz" >&2
	exit 1
fi
tar -xvzf /tmp/gempak-${package_version}.tar.gz -C /home/gempak >& /dev/null
rm -rf /home/gempak/GEMPAK7
mv "/home/gempak/${srcdir}" /home/gempak/GEMPAK7
pushd /home/gempak
cd GEMPAK7
. Gemenviron.profile
export PYINC=""
export PYLIB=""
export WITHPY=""
export PYDEP=""
export LDFLAGS="-L$OS_LIB -s"

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
rm -rf .gitignore .github

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
