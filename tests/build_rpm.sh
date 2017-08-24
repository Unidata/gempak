#!/bin/sh -xe
. ~/GEMPAK7/Gemenviron.profile
cd $NAWIPS

# Prepare the RPM environment
mkdir -p /tmp/rpmbuild/{BUILD,RPMS,SOURCES,SPECS,SRPMS}

cp $NAWIPS/rpm/Installer.gempak/linux64.spec /tmp/rpmbuild/SPECS
package_version=`grep "define version" rpm/Installer.gempak/linux64.spec | grep -v version_core| awk '{print $3}'`
git archive --format=tar --prefix=gempak-${package_version}/ HEAD  | gzip >/tmp/rpmbuild/SOURCES/gempak-${package_version}.tar.gz

# Build the RPM
rpmbuild --define '_topdir /tmp/rpmbuild' -ba /tmp/rpmbuild/SPECS/linux64.spec

# After building the RPM, try to install it
#yum localinstall -y /tmp/rpmbuild/RPMS/x86_64/gempak*




