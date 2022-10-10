#!/bin/csh -f

set FILES=`ls grid*.gif`

set PROG='montage -tile 3x5 -mode frame -geom 350x250 -pen black -bordercolor black'
foreach FILE ($FILES)
   set GRID=`echo $FILE:r | cut -c5-`
   @ GNUM = $GRID
   echo $GNUM
   set PROG="$PROG -label Grid${GNUM} $FILE"
end
set PROG="$PROG test.jpg"
$PROG
