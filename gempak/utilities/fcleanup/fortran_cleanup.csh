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
   if ( -l $FILE ) continue
   if ( $FILE == "fdf" ) continue

   if ( -d $FILE ) then
      cd $FILE
      $GEMPAK/utilities/fcleanup/fortran_cleanup.csh $FILE
      cd ..
   else
      set EXT=$FILE:e
      if ( $EXT == "f" ) then
         $GEMPAK/utilities/fcleanup/fcleanup $FILE >! $FILE.cleanup
         set STATUS=$status
	 if ( $STATUS != 0 ) then
	    echo "----- length problem $FILE in " `pwd`
         endif
         cmp -s $FILE $FILE.cleanup
         set STATUS=$status
         if ( $STATUS == 0 ) then
            rm $FILE.cleanup
         else
            mv $FILE $FILE.cleanup_sav
            mv $FILE.cleanup $FILE
            echo "check $FILE in " `pwd`
         endif
      endif
   endif
end

