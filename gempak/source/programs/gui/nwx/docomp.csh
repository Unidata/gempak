#!/bin/csh -f

set OLDDIR=~gempak/GEMPAK5.9.3/gempak/source/programs/gui/nwx

set FILES=`ls *.[fhc]`
foreach FILE ($FILES)
   echo FILE $FILE
   if ( ! -e $OLDDIR/$FILE ) then
      echo "NEW: $FILE"
   else
      diff $FILE $OLDDIR
   endif
end
