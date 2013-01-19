#!/bin/csh -f

set FILES=`grep -v ^! $GEMTBL/nmap/mod_res.tbl | tr -s " " | cut -f2 -d" " | cut -f2- -d/`

foreach FILE ($FILES)   
   if ( ! -e $NMAP_RESTORE/$FILE ) echo "missing $NMAP_RESTORE/$FILE"
end
