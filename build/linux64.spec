#
# Unidata GEMPAK Spec File
# 
# Nov 22, 2016  mjames  7.3.2
#
%define __prelink_undo_cmd %{nil}
%define gem_home /machine/GEMPAK7
%define prefix /home/gempak
%define version 7.3.2
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
BuildRequires: openmotif, openmotif-devel
AutoReq: no
provides: gempak

%description
Unidata GEMPAK Distribution

%prep
# This is a somewhat manual RPM build where we copy the latest source code 
# release from github, but bundle the $NAWIPS/os directory (there is no 
# make performed by this spec file).

%build

%install
WORKSPACE_DIR="/machine/rpmbuild"
SOURCE_DIR=${WORKSPACE_DIR}/SOURCES/%{version_core}/
SPEC_DIR=${WORKSPACE_DIR}/SPECS/
SRC_ZIP=master.zip

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

# wget the latest source
cd ${RPM_BUILD_ROOT}/home/gempak
if [ ! -f %{gem_home}/${SRC_ZIP} ]; then
  wget -O ${RPM_BUILD_ROOT}/home/gempak/${SRC_ZIP} https://github.com/Unidata/gempak/archive/master.zip
else
  cp %{gem_home}/${SRC_ZIP} .
fi
unzip ${SRC_ZIP}
rm -rf ${SRC_ZIP}
mv gempak-master GEMPAK%{version_core}
cd GEMPAK%{version_core}/
cp -r %{gem_home}/os ${RPM_BUILD_ROOT}/home/gempak/GEMPAK%{version_core}/

# create soft link to the current gempak directory
cd ..
ln -s GEMPAK%{version_core} NAWIPS

%pre
echo -e "\e[1;34m\| Installing GEMPAK%{version_core}...\e[m"

%post
echo -e "\e[1;32m\| GEMPAK%{version_core} Installation - COMPLETE\e[m"

# TODO: re-write Gemenviron files for prefix given on install
#sed -i 's/home\/gempak/${PREFIX_ESC}/g' %{prefix}/GEMPAK%{version_core}/Gemenviron
#sed -i 's/home\/gempak/${PREFIX_ESC}/g' %{prefix}/GEMPAK%{version_core}/Gemenviron.profile

%postun
echo -e "\e[1;34m\| GEMPAK%{version_core} Has Been Successfully Removed\e[m"

%clean
rm -rf ${RPM_BUILD_ROOT}/*

%files
%defattr(-,gempak,-,-)
%dir %{prefix}/GEMPAK%{version_core}/
%{prefix}/GEMPAK%{version_core}/*
%attr(755,gempak,-) %{prefix}/NAWIPS

