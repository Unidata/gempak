#!/bin/sh -xe
OS_TYPE=$1
OS_VERSION=$2

ls -l /home

# Clean the yum cache
yum -y clean all
yum -y clean expire-cache
yum groupinstall "Development tools" -y >& /dev/null
yum install libxslt git rpm-build openmotif-devel gcc gcc-c++ gcc-gfortran libX11-devel libXt-devel libXext-devel libXp-devel libXft-devel libXtst-devel xorg-x11-xbitmaps flex byacc *fonts-ISO8859-* python-devel -y >& /dev/null

# Prepare the RPM environment
mkdir -p /tmp/rpmbuild/{BUILD,RPMS,SOURCES,SPECS,SRPMS}
pushd /gempak
cp build/Installer.gempak/gempak.spec /tmp/rpmbuild/SPECS
package_version=`grep "define version" build/Installer.gempak/gempak.spec | grep -v version_core| awk '{print $3}'`
git archive --format=tar --prefix=GEMPAK7/ HEAD  | gzip >/tmp/rpmbuild/SOURCES/gempak-${package_version}.tar.gz
popd

# Build the RPM
useradd gempak

rpmbuild --define '_topdir /tmp/rpmbuild' -ba /tmp/rpmbuild/SPECS/gempak.spec

# After building the RPM, try to install it
# Fix the lock file error on EL7.  /var/lock is a symlink to /var/run/lock
mkdir -p /var/run/lock

yum localinstall -y /tmp/rpmbuild/RPMS/x86_64/gempak*

cp /tmp/rpmbuild/SOURCES/gempak-${package_version}.tar.gz /gempak/build/
cp /tmp/rpmbuild/RPMS/x86_64/gempak*.rpm /gempak/build/
