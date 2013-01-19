#!/bin/csh

cd $NAWIPS

mkdir os/$NA_OS/bin
mkdir os/$NA_OS/include
mkdir os/$NA_OS/lib
mkdir os/$NA_OS/man
mkdir os/$NA_OS/share

cd os/$NA_OS/include
ln -s ../../../gempak/include/MCHPRM.$OS MCHPRM.PRM
