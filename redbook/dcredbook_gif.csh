#!/bin/csh -f
#
# Filename: dcredbook_gif.csh
#
# This script may be used as a pqact.conf action to automatically
# generate GIF output from REDBOOK graphics found on NOAAPORT.
# Place this script in the ~ldm/util directory and make the
# script executble with "chmod a+x ~ldm/util/dcredbook_gif.csh".
#
# pqact.conf usage:
# 
# send all NOAAPORT graphic products (except those from KWAL) to this script
#HRS     ^P..... (K[^W]|KW[^A]|KWA[^L])
#       PIPE    -close  util/dcredbook_gif.csh
#
#
# source the GEMPAK environment so that table, mapfiles etc. are found
#
source /home/gempak/NAWIPS/Gemenviron
#
#
# The gf driver needs an X server to display to.
setenv DISPLAY laraine:1
#
#
#
# GIFDIR is where we want to place the GIF output
setenv GIFDIR $GEMDATA/redbook/gifs
#
# OUTDIR is optional for use with the -s flag to save the 
# raw redbook graphics to appropriately named files
setenv OUTDIR $GEMDATA/redbook/data
#
#
cd $GIFDIR
#
# Take the redbook graphic from stdin and pipe to the dcredbook
# decoder. 
cat | dcredbook_gf -v 1 -d $GEMDATA/logs/dcredbook_gif.log  \
	-s $OUTDIR/%P_${1}_${2}_${3} \
	'gf|%P-YYYYMMDD_HHNN.gif|1280;1024'
#
exit 0
