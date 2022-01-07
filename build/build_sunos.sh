#!/bin/bash 
# 
# Build script for GEMPAK on SunOS
# BuildRequires: automake
#
# Author: Michael James <mjames@ucar.edu>
#
# Aug 29, 2017   Created for GEMPAPK 7.4.0 on Solaris 10 (motherlode)
#

. /home/gempak/GEMPAK7/Gemenviron.profile
cd $NAWIPS

# /opt/csw/gnu should be first to find the correct m4, required 
# for extlibs/netCDF build
export PATH=/opt/csw/gnu:/usr/ccs/bin:$PATH

# Prepare the RPM environment
package_version=`grep "define version" build/Installer.gempak/gempak.spec | grep -v version_core| awk '{print $3}'`

make extlibs
make gempak
make install
make programs_gf
make programs_nc
make clean


BUILD_DIR=/tmp/gempak-solaris-${package_version}.x86/

if [ -d ${BUILD_DIR} ]; then
   rm -rf ${BUILD_DIR}
fi
mkdir -p ${BUILD_DIR}

if [ -d ${BUILD_DIR} ]; then

  cd ${BUILD_DIR}
  mkdir GEMPAK7
  pushd GEMPAK7
  rsync -av --progress /home/gempak/GEMPAK7/ . --exclude .git --exclude build --exclude extlibs --exclude config  --exclude .github --exclude .gitignore --exclude Makefile
  #rm -rf extlibs config .gitignore .github build
  popd
  ln -s GEMPAK7 NAWIPS
  tar -cf gempak-${package_version}.sol.x86.tar GEMPAK7 NAWIPS
  gzip -9 gempak-${package_version}.sol.x86.tar 
  mv gempak-${package_version}.sol.x86.tar.gz $NAWIPS/build/dist/

fi
