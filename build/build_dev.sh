#!/bin/sh -xe
##
# GEMPAK container source build
# Author: mjames@ucar.edu
#
# Make changes to source code outside of this container and build the updates within
#
##

# /home/gempak/GEMPAK7 already exists 

rsync -ruql --delete --exclude=os* --exclude=extlibs /gempak/* /home/gempak/GEMPAK7
ls -la /home/gempak/GEMPAK7/

. /home/gempak/GEMPAK7/Gemenviron.profile
cd $NAWIPS
make clean
make gempak
make install
ls -la $OS_BIN
