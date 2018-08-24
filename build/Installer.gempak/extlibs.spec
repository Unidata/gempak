# Unidata GEMPAK Extlib Spec File
%define __prelink_undo_cmd %{nil}
%define gem_home /home/gempak/GEMPAK7
%define prefix /home/gempak
%define version 7.5.1
Name: gempak-extlibs
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
Packager: mjames@ucar.edu
BuildRequires: libX11-devel, libXt-devel, libXext, libXp-devel
BuildRequires: libXft-devel, libXtst-devel, xorg-x11-xbitmaps
BuildRequires: flex, byacc, openmotif-devel
Requires: openmotif, libX11, libXt, libXext, libXp, libXft, libXtst,  xorg-x11-xbitmaps
AutoReq: no
provides: gempak-extlibs

%description
Unidata GEMPAK Extlib Distribution

%prep
mkdir -p %{prefix}
pushd %{prefix}
tar -xvzf /tmp/rpmbuild/SOURCES/gempak-%{version}.tar.gz -C %{prefix}/ >& /dev/null

%build

%install
# create build root directory
pushd %{gem_home}
export NAWIPS=`pwd`
. build/Installer.gempak/Gemenviron.profile
make distclean
make extlibs
make clean >& /dev/null
mkdir -p ${RPM_BUILD_ROOT}/home/gempak/GEMPAK7
mv os ${RPM_BUILD_ROOT}/home/gempak/GEMPAK7

%pre

%post

%postun

%clean

%files
%defattr(-,gempak,-,-)
%{prefix}/GEMPAK7

