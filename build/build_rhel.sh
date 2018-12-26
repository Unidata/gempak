#!/bin/sh -xe

# Prepare the RPM environment
mkdir -p /tmp/rpmbuild/{BUILD,RPMS,SOURCES,SPECS,SRPMS}

pushd /gempak

cp build/Installer.gempak/gempak.spec /tmp/rpmbuild/SPECS
package_version=`grep "define version" build/Installer.gempak/gempak.spec | grep -v version_core| awk '{print $3}'`
git archive --format=tar --prefix=GEMPAK7/ HEAD  | gzip >/tmp/rpmbuild/SOURCES/gempak-${package_version}.tar.gz

popd

rpmbuild --define '_topdir /tmp/rpmbuild' -ba /tmp/rpmbuild/SPECS/gempak.spec

# After building the RPM, try to install it
#yum localinstall -y /tmp/rpmbuild/RPMS/x86_64/gempak*.rpm

cp /tmp/rpmbuild/SOURCES/gempak-${package_version}.tar.gz /gempak/build/dist/
cp /tmp/rpmbuild/RPMS/x86_64/gempak*.rpm /gempak/build/dist/
