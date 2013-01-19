#!/bin/csh

if  ( $#argv < 1 )  then
    echo " "
    echo " Usage: `basename $0` release_version"
    echo "         release_version is the version number, e.g., 6.6.0"
    echo " "
    exit
endif

if  ( `whoami` == "nawopr" )  then

    cd $HOME

    set vers=${1}

    cp ~nawcm/tarfiles/source_release${vers}.tar.gz .

    zcat source_release${vers}.tar.gz | tar xvf -

    rm -f source_release${vers}.tar.gz

endif
