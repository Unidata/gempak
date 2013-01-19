#!/bin/csh -f

set JDAY=`date -u +'%j'`
set FILES=`ls /data/ldm/fsl/RASS/06min/2005${JDAY}*.nc`
foreach FILE ($FILES)
   cat $FILE | dcncprof -v -l - YYYYMMDD_rass.gem
end

set FILES=`ls /data/ldm/fsl/06min/2005${JDAY}*.nc`
foreach FILE ($FILES)
   cat $FILE | dcncprof -v -l - YYYYMMDD_rass.gem
end
