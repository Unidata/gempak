#!/bin/csh -f
source /home/gempak/NAWIPS/Gemenviron
setenv DISPLAY :0.0
set SITE=$1
set TYPE=$2
set DATTIM=$3
set FILENAME=${TYPE}_${DATTIM}
set GIFFILE=${SITE}_${TYPE}_${DATTIM}.gif
set WEBDIR=/local/ldm/data/gempak/web

# define these three if you want to push to a remote server
set REMOTE_WEB_DIR=remote_path
set REMOTE_USER=remote_user
set REMOTE_HOST=remote_host

# Make sure the NEXRAD file exists before seting lock
#
@ COUNT = 0
while (! -e $RAD/NIDS/${SITE}/${TYPE}/${FILENAME} )
   sleep 2
   @ COUNT = $COUNT + 1
   if($COUNT == 10) then
      exit 0
   endif
end

# change to working directory 
if ( ! -e $WEBDIR ) mkdir -p $WEBDIR
cd $WEBDIR

# set lock file
set LOCK=.inuse.$$
touch $LOCK

@ COUNT = 0
set TEST=`ls -rt .inuse.* | head -1`
set OFFENDING=$TEST

# wait for other processes to finish...
while(($TEST != $LOCK)&&($COUNT < 51))
   sleep 4
   set TEST=`ls -rt .inuse.* | head -1`
   if ( ( $COUNT == 40 ) && ( $TEST == $OFFENDING ) ) then
      # this lock has been around a really long time. Maybe its toast.
      rm -f $OFFENDING
   endif
   if($COUNT == 50) then
      echo "Please check `hostname` on $TEST for nids generation $SITE $TYPE $DATTIM" | \
         logger -t "$0 [$$]" -p local0.notice
      rm $LOCK
      exit 0
   endif
   @ COUNT = $COUNT + 1
end
if(-e ${GIFFILE}) rm -f $GIFFILE

# create title string
set PRODUCT = `grep ${TYPE} ${GEMTBL}/nmap/nidsid.tbl | cut -d"," -f 2`
set TITLE="NIDS $SITE $TYPE $PRODUCT $DATTIM"

# run gpnids
gpnids_gif << EOF
 RADFIL   = \$RAD/NIDS/${SITE}/${TYPE}/${FILENAME}
 RADTIM   = last
 GAREA    = dset
 DEVICE   = gif|${GIFFILE}|900;850
 MAP      = 7/1/1 + 8/1/1
 \$mapfil = hicnus.nws+histus.nws
 LATLON   = 0
 PANEL    = 0
 TITLE    = 1/-2/${TITLE}
 TEXT     = 0.7//////sw
 COLORS   = 7=38:38:38;8=112:112:112
 LINE     = 2;3//2
 IMCBAR   = 1
 CLEAR    = y
 LUTFIL   = default
 STNPLT   = 5|0|disco_sites.tbl
 r

 e
EOF

# rename and SCP to remove host
set LAST=${SITE}_${TYPE}
if(-e $GIFFILE) then
   if(-e $LAST.gif) rm -f $LAST.gif
   mv $GIFFILE $LAST.gif
   scp $LAST.gif $REMOTE_USER@$REMOTE_HOST:$REMOTE_WEB_DIR
endif

# clean up
rm -f $LOCK
rm -f *.nts
exit 0
