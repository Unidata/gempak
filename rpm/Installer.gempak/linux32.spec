#
# GEMPAK LDM Spec File
#
%define __prelink_undo_cmd %{nil}
%define prefix /home/gempak
%define version 7.4.0
%define version_core 7
Name: gempak 
Summary: Unidata Program Center GEMPAK
Version: %{version}
Release: 1 
Prefix: %{prefix}
Group: GEMPAK
BuildRoot: /tmp
URL: N/A
License: N/A
Distribution: N/A
Vendor: Unidata
Packager: Michael James
Requires: libX11-devel, libXt-devel, libXext, libXp-devel
Requires: libXft-devel, libXtst-devel, xorg-x11-xbitmaps
Requires: flex, byacc
AutoReq: no
provides: gempak

%description
GEMPAK Distribution

%prep
# We cannot safely build gempak on a machine gempak
# is already installed on.
if rpm -q gempak
then
   echo "ERROR: the gempak rpm is already installed"
   exit 1
fi

%build

%install
# define source package
WORKSPACE_DIR="/home/mjames/rpmbuild"
GEM_TAR_DIR=$WORKSPACE_DIR/SOURCES/7

GEM_TAR_FILE="master.zip"

# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

# create build root directory - not needed since we are unzipping the source
if [ -d ${RPM_BUILD_ROOT}/home/gempak ]; then
   rm -rf ${RPM_BUILD_ROOT}/home/gempak
fi
mkdir -p ${RPM_BUILD_ROOT}/home/gempak

# Copy the base src to the build directory.
cp -r ${GEM_TAR_DIR}/${GEM_TAR_FILE} ${RPM_BUILD_ROOT}/home/gempak/${GEM_TAR_FILE}
cd ${RPM_BUILD_ROOT}/home/gempak
unzip ${GEM_TAR_FILE}
rm -rf ${GEM_TAR_FILE}
mv gempak-master GEMPAK%{version_core}
cd GEMPAK%{version_core}/
cp ${GEM_TAR_DIR}/linux32.os.tar ${RPM_BUILD_ROOT}/home/gempak/GEMPAK%{version_core}/
tar -xvf linux32.os.tar
rm -rf linux32.os.tar

# remove external libraries and OS tarball
rm -rf extlibs

# create soft link to the current gempak directory
cd ..
ln -s GEMPAK%{version_core} NAWIPS

%pre
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| Installing GEMPAK%{version_core}...\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%post
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;32m\| GEMPAK%{version_core} Installation - COMPLETE\e[m"
echo -e "\e[1;32m--------------------------------------------------------------------------------\e[m"

# re-write Gemenviron files for prefix given on install
#sed -i 's/home\/gempak/${PREFIX_ESC}/g' %{prefix}/GEMPAK%{version_core}/Gemenviron
#sed -i 's/home\/gempak/${PREFIX_ESC}/g' %{prefix}/GEMPAK%{version_core}/Gemenviron.profile

%postun
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"
echo -e "\e[1;34m\| GEMPAK%{version_core} Has Been Successfully Removed\e[m"
echo -e "\e[1;34m--------------------------------------------------------------------------------\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}/*

%files
%defattr(-,gempak,-,-)
%dir %{prefix}/GEMPAK%{version_core}/
%{prefix}/GEMPAK%{version_core}/*
%attr(755,gempak,-) %{prefix}/NAWIPS
