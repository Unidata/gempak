#!/bin/csh

if  ( $#argv < 1 )  then
    echo " "
    echo " Usage: `basename ${0}` account_name"
    echo "        where account_name is nawips1, nawips2 or nawips3"
    echo " "
    exit (1)
endif

if  ( `whoami` != "nawopr" )  then
    echo " "
    echo " WARNING:"
    echo " This script must be executed from the nawopr account."
    echo " "
    exit (1)
endif

cd $HOME

tar --preserve -cvf \
	bin doc icons ldm resource scripts \
	gempak/{climo,data,error,fonts,help,include,maps,nts,pdf,tables} \
	exe lib versions -  |
    ssh -l ${1} ncosrv tar -C /ops/ws/${1} -xvf -
