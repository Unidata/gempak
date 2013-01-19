#!/bin/csh -f

if ( ! -e mod_res.list ) then
   echo "Cannot find mod_res.list file"
   exit
endif

set OUTFILE=mod_res.combined
touch $OUTFILE

set FILES=`grep -v ^\! mod_res.list`
foreach FILE ($FILES)
   cat $FILE | awk -f mod_res.awk  >> $OUTFILE
end

if ( -e mod_res.tbl ) mv mod_res.tbl mod_res.tbl.$$
mv $OUTFILE mod_res.tbl
