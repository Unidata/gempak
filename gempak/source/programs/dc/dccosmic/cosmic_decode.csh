#!/bin/csh -f

source ~gempak/NAWIPS/Gemenviron

cd ~ldm/data/cosmic

if ( -e .in_progress ) then
   rm .in_progress
   exit 1
endif
touch .in_progress

if ( ! -e .decoded ) touch .decoded

touch .decoded_new
set FILES=`find . -name 'wetPrf_*' -newer .decoded -print`

if ( $#FILES < 1 ) then
   rm .decoded_new
   rm .in_progress
   exit 0
endif

foreach FILE ($FILES)
   echo FILE $FILE
   dccosmic -v -l $GEMDATA/logs/dccosmic.log -b 30 -f $FILE $GEMDATA/cosmic/YYYYMMDDHHNN_cosmic.gem
   # 
   # as long as we are making progress, keep touching progress semaphore
   touch .in_progress
end
mv .decoded_new .decoded

rm .in_progress
