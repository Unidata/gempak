#!/bin/sh -xe
# Prepare the RPM environment
mkdir -p /tmp/rpmbuild/{BUILD,RPMS,SOURCES,SPECS,SRPMS}
pushd /gempak
cp build/Installer.gempak/fedora.spec /tmp/rpmbuild/SPECS
package_version=`grep "define version" build/Installer.gempak/fedora.spec | grep -v version_core| awk '{print $3}'`

# Create archive to use in fedora.spec
git archive --format=tar --prefix=GEMPAK7/ HEAD  | gzip >/tmp/rpmbuild/SOURCES/gempak-${package_version}.tar.gz
popd

rpmbuild --define '_topdir /tmp/rpmbuild' -ba /tmp/rpmbuild/SPECS/fedora.spec

# After building the RPM, try to install it
# Fix the lock file error on EL7.  /var/lock is a symlink to /var/run/lock
#mkdir -p /var/run/lock
#yum localinstall -y /tmp/rpmbuild/RPMS/x86_64/gempak*

cp /tmp/rpmbuild/SOURCES/gempak-${package_version}.tar.gz /gempak/build/dist/
cp /tmp/rpmbuild/RPMS/x86_64/gempak*.rpm /gempak/build/dist/
