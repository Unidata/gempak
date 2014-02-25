#!/bin/csh -f

source /home/gempak/Gemenviron

set TOP=$cwd
set REDBOOK=$GEMDATA/afos/rbk

cd $REDBOOK
set DIRS=`ls`

cd $TOP
if(! -e work) mkdir work

foreach DIR ($DIRS)
   cd $REDBOOK/$DIR
   set FILE=`ls | tail -1`
   if($#FILE > 0) then
      echo dir $DIR $FILE
      cd $TOP/work
      #cat $REDBOOK/$DIR/$FILE | dcredbook_gf -d - 'GF|%P.gif'
   endif
end

