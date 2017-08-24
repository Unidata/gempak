#
# Unidata GEMPAK Spec File
# 
# Nov 22, 2016  mjames  Created
# Aug 24, 2017  mjames  Modified for docker rpm builds w traviscl
#
%define __prelink_undo_cmd %{nil}
%define gem_home /home/gempak/GEMPAK7
%define prefix /home/gempak
%define version 7.4.0
Name: gempak
Summary: Unidata GEMPAK
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

%build

%install
# create build root directory
if [ -d ${RPM_BUILD_ROOT}%{gem_home} ]; then
   rm -rf ${RPM_BUILD_ROOT}%{gem_home}
fi
mkdir -p ${RPM_BUILD_ROOT}%{gem_home}
cd ${RPM_BUILD_ROOT}%{gem_home}
cp -r /gempak/* .
export NAWIPS=`pwd`
. rpm/Installer.gempak/Gemenviron.profile
make all
make install
make programs_gf
make programs_nc
make clean

# create soft link to the current gempak directory
cd ..
ln -s GEMPAK7 NAWIPS

%pre

%post

%postun

%clean

%files
%defattr(-,gempak,-,-)
%dir %{prefix}/GEMPAK7/
%{prefix}/GEMPAK7/*
%attr(755,gempak,-) %{prefix}/NAWIPS

