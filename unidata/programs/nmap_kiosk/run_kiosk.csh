#!/bin/csh -f
#
# This script is used to run the nmap_kiosk program which
# will automatically change the nmap2 loop frame at set
# intervals. Define the FIRST and LAST loop frames
# (1 to 16 are available) and the time to SLEEP
# before changing the loop frames.
#
# these are the first loop frame and last loop frame 
# in the sequence to cycle through
set FIRST=1
set LAST=3
#
# This is the time to sleep in between changing loops
set SLEEP=30

echo "select NMAP window"
set windowid=`xwininfo  | grep NMAP2 | cut -d" " -f4`

echo "Running Kiosk for NMAP2 with windowid $windowid"
echo "ctrl-c to exit"
exec nmap_kiosk -t $SLEEP -w $windowid $FIRST $LAST

