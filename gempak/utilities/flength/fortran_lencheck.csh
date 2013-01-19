#!/bin/csh -f

if ($#argv < 1) then
   set TOP=`pwd`
else
   set TOP=$1
endif

echo "Processing from $TOP"
cd $TOP

set FILES=`ls`

foreach FILE ($FILES)
   if (-l $FILE ) continue
   if ( $FILE == "fdf" ) continue
   if ( -d $FILE ) then
      cd $FILE
      $GEMPAK/utilities/flength/fortran_lencheck.csh $FILE
      cd ..
   else
      set EXT=$FILE:e
      if ( $EXT == "f" ) then
         $GEMPAK/utilities/flength/flength $FILE 
         set STATUS=$status
         if ( $STATUS != 0 ) echo "check line length $cwd/$FILE"
         endif
      endif
    endif
end


