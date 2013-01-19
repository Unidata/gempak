#!/bin/csh -f
###########################################################
#
# This script will use the LDM newlog facility to rotate
# the GEMPAK log files found in the $GEMDATA/logs directory
#
# Steve Chiswell	 2/2003
###########################################################

source ~gempak/Gemenviron

cd $GEMDATA/logs

set LOGS=`ls *.log`

foreach LOG ($LOGS)
   echo rotate $LOG
   ~ldm/bin/newlog ./$LOG 2
end
