#!/bin/csh

# define source package
set base_version=7
#set version="7.2.0"
set version=`grep "vmsg =" /home/gempak/NAWIPS/gempak/source/gemlib/ss/ssvers.f | cut -d " " -f4 | sed "s/'//"`
set WORKSPACE_DIR=/machine/rpmbuild/SPECS
set GEM_TAR_DIR=/machine/rpmbuild/SOURCES/${base_version}
set GEM_TAR_FILE="gempak-${version}.tar.gz"
set GEM_X86_FILE="sol_x86.os.tar"

# Copy the base src to the build directory.
cd ${WORKSPACE_DIR}
cp -r ${GEM_TAR_DIR}/${GEM_TAR_FILE} .
tar -xvzf ${GEM_TAR_FILE}
rm -rf ${GEM_TAR_FILE}
cd GEMPAK${version}
cp ${GEM_TAR_DIR}/${GEM_X86_FILE} .
tar -xvf ${GEM_X86_FILE}
rm -rf ${GEM_X86_FILE}

# remove external libraries and OS tarball
rm -rf extlibs


