#!/bin/csh

if  ( $#argv < 2 )  then
    echo " "
    echo " Usage: `basename $0` yyyymmdd distb_pw"
    echo " "
    exit (1)
endif

./ftp_distb.csh << EOF1
aix5
execs
$1
$2
EOF1

./ftp_distb.csh << EOF2
hpux11
execs
$1
$2
EOF2

./ftp_distb.csh << EOF3
linux2.4el
execs
$1
$2
EOF3

./ftp_distb.csh << EOF3
linux2.4
exeonly
$1
$2
EOF3

