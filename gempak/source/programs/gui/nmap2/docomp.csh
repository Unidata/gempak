#!/bin/csh -f

set FILES=`ls *.c`
foreach FILE ($FILES)
 if ( -e $GEMPAK/source/programs/gui/nmap2/$FILE ) then
    echo "---- $FILE ----"
    diff $FILE $GEMPAK/source/programs/gui/nmap2 
 endif
end

