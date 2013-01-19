#!/bin/csh -f

if($#argv != 3) then
   echo 'Usage: buildmacro.csh OPSYS GEMPRM INCDIR'
   exit
endif

if($2 == 'GEMPRM.PRM') then
   set PARM="gemprm.${1}"
else
   set PARM=`echo $2 | tr '[:upper:]' '[:lower:]'`
endif

ln -s ${3}/${PARM} $2
ln -s $2 './GEMINC:'${2}
