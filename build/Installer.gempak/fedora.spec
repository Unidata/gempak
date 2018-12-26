#
# Unidata GEMPAK Spec File
#
# The GEMPAK RPM appears to require being build in /home/gempak
# (rather than ${BUILD_RPM_ROOT}/home/gempak due to static linking.
# 
# Nov 22, 2016  mjames  Created
# Aug 24, 2017  mjames  7.4.0 docker rpm builds w traviscl
#
%define __prelink_undo_cmd %{nil}
%define gem_home /home/gempak/GEMPAK7
%define prefix /home/gempak
%define version 7.5.1
Name: gempak
Summary: Unidata GEMPAK
Version: %{version}
Release: 1%{?dist}
Prefix: %{prefix}
Group: GEMPAK
BuildRoot: /tmp
URL: https://www.unidata.ucar.edu/software/gempak/
License: Open Source
Distribution: N/A
Vendor: Unidata
Packager: Michael James
BuildRequires: libX11-devel, libXt-devel, libXext, libXp-devel
BuildRequires: libXft-devel, libXtst-devel, xorg-x11-xbitmaps
BuildRequires: flex, byacc, openmotif-devel
Requires: openmotif, libX11, libXt, libXext, libXp, libXft, libXtst,  xorg-x11-xbitmaps
AutoReq: no
provides: gempak

%description
Unidata GEMPAK Distribution

%prep
mkdir -p %{prefix}
pushd %{prefix}
tar -xvzf /tmp/rpmbuild/SOURCES/gempak-%{version}.tar.gz -C %{prefix}/ >& /dev/null

%build

%install
# create build root directory
pushd %{gem_home}
export NAWIPS=`pwd`
cat source_python.sh
cat source_python.sh >> build/Installer.gempak/Gemenviron.profile
. build/Installer.gempak/Gemenviron.profile
# TODO: ". source_python.sh" doesn't seem to work from spec file

pushd config
rm -rf Makeinc.linux64_gfortran
ln -s Makeinc.linux64_gfortran_fedora Makeinc.linux64_gfortran
popd

make gempak #>& make.gempak
make install >& /dev/null
make programs_gf >& /dev/null
make programs_nc >& /dev/null
make clean >& /dev/null

#grep -i error make.gempak
#rm -rf make.gempak

mkdir -p ${RPM_BUILD_ROOT}/home/gempak/
cd ..
mv GEMPAK7 ${RPM_BUILD_ROOT}/home/gempak/
cd ${RPM_BUILD_ROOT}/home/gempak/GEMPAK7
rm -rf extlibs .gitignore .travis.yml build

%pre

%post
ln -s %{prefix}/GEMPAK7 %{prefix}/NAWIPS
chown gempak %{prefix}/NAWIPS

%postun

%clean

%files
%defattr(-,gempak,-,-)
%{prefix}/GEMPAK7

