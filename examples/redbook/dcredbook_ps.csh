#!/bin/csh -f
#
# Filename: dcredbook_ps.csh
#
# This scripts may be used as a pqact.conf action to automatically
# print REDBOOK graphics found on NOAAPORT.
#
# pqact.conf usage:
# 
# send all NOAAPORT graphic products (except those from KWAL) to this script
#HRS     ^P..... (K[^W]|KW[^A]|KWA[^L])
#	PIPE    -close  util/dcredbook_ps.csh
#
#
# source the GEMPAK environment so that table, mapfiles etc. are found
#
source /home/gempak/NAWIPS/Gemenviron

# Set a work directory and change directory to it
setenv PSDIR $GEMDATA/web/redbook
cd $PSDIR

#cat | dcredbook_ps -v 1 -d $GEMDATA/logs/dcredbook.log 'PS|%P.ps||C'

# set he GEMPAK device line, use the process number of this script as the file ID.
set DEV='PS|'$$'.ps||M'

# Redirect standard input to the dcredbook_ps decoder
cat | dcredbook_ps -v 1 -d $GEMDATA/logs/dcredbook.log $DEV

# print and remove the postscript file (use -c to copy the file to spool)
lp -c -dhp $$.ps
rm $$.ps

exit 0
