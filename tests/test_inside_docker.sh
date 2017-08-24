#!/bin/sh -xe

OS_VERSION=$1

ls -l /home

# Clean the yum cache
yum -y clean all
yum -y clean expire-cache
yum install libxslt git rpm-build openmotif-devel gcc gcc-c++ gcc-gfortran libX11-devel libXt-devel libXext-devel libXp-devel libXft-devel libXtst-devel xorg-x11-xbitmaps flex byacc *fonts-ISO8859-* python-devel -y

# Prepare the RPM environment
mkdir -p /tmp/rpmbuild/{BUILD,RPMS,SOURCES,SPECS,SRPMS}
cat >> /etc/rpm/macros.dist << EOF
%dist .osg.el${OS_VERSION}
%osg 1
EOF

cp gempak/rpm/Installer.gempak/linux64.spec /tmp/rpmbuild/SPECS
ln -s /home/gempak/GEMPAK7 gempak
pushd gempak
package_version=`grep "define version" rpm/Installer.gempak/linux64.spec | grep -v version_core| awk '{print $3}'`
git archive --format=tar --prefix=gempak-${package_version}/ HEAD  | gzip >/tmp/rpmbuild/SOURCES/gempak-${package_version}.tar.gz
popd

# Build the RPM
rpmbuild --define '_topdir /tmp/rpmbuild' -ba /tmp/rpmbuild/SPECS/linux64.spec

# After building the RPM, try to install it
# Fix the lock file error on EL7.  /var/lock is a symlink to /var/run/lock
mkdir -p /var/run/lock

yum localinstall -y /tmp/rpmbuild/RPMS/x86_64/gempak*




