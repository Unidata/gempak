#!/bin/csh -f

if (! $?GEMTBL) then
   echo set GEMTBL environmental variable before running script
   exit
endif

if (! $?GEMPAK) then
   echo set GEMPAK environmental variable before running script
   exit
endif

echo "Generating pqact.gempak file using:"
echo "   GEMTBL = $GEMTBL"
echo "   GEMPAK = $GEMPAK"

set NAWIPS_LDM=$NAWIPS/ldm/etc
#
# The following creates DC decoder entries with GEMTBL and GEMPAK defined
# for the local system
#
set PQACT=1

if ( $PQACT ) then
   set DECODERS = pqact.gempak
   set NWX = pqact.gempak
   set IMAGES = pqact.gempak
   set NEXRAD = pqact.gempak
   set CRAFT = pqact.gempak
   set UPC = pqact.gempak
else
   set DECODERS = pqact.gempak_decoders
   set NWX = pqact.gempak_nwx
   set IMAGES = pqact.gempak_images
   set NEXRAD = pqact.gempak_nexrad
   set CRAFT = pqact.gempak_craft
   set UPC = pqact.gempak_upc
endif

if ( ( ! $PQACT ) && ( $cwd != $NAWIPS_LDM/templates ) ) then
   if ( -e $DECODERS ) rm -f $DECODERS
   if ( -e $NWX ) rm -f $NWX
   if ( -e $IMAGES ) rm -f $IMAGES
   if ( -e $NEXRAD ) rm -f $NEXRAD
   if ( -e $CRAFT ) rm -f $CRAFT
   if ( -e $UPC ) rm -f $UPC
endif


cat $NAWIPS_LDM/templates/pqact.gempak_decoders_grid | sed 's@\@GEMTBL\@@'${GEMTBL}'@g' | \
   sed 's@\@GEMPAK\@@'${GEMPAK}'@g' >! $DECODERS

cat $NAWIPS_LDM/templates/pqact.gempak_decoders.in | sed 's@\@GEMTBL\@@'${GEMTBL}'@g' | \
   sed 's@\@GEMPAK\@@'${GEMPAK}'@g' >>! $DECODERS
#
# NWX FILE actions
#
cat $NAWIPS_LDM/templates/pqact.gempak_nwx >>! $NWX

#
# Unidata/Wisconsin Images
#
cat $NAWIPS_LDM/templates/pqact.gempak_images >>! $IMAGES

#
# NEXRAD actions
#
cat $NAWIPS_LDM/templates/pqact.gempak_nexrad >>! $NEXRAD

#
# CRAFT actions
cat $NAWIPS_LDM/templates/pqact.gempak_craft >>! $CRAFT

#
# Unidata local actions
#
#cat templates/pqact.gempak_upc >>! $UPC

echo ' '
echo '################################################################'
echo 'Place the generated PQACT files into ~ldm/etc.'
echo 'Use the following entries for the LDM ldmd.conf file: '
echo ' '
if ( $PQACT ) then
   echo 'exec	"pqact etc/pqact.gempak"'
else
   echo 'exec	"pqact -f ANY-NNEXRAD-CRAFT-NIMAGE etc/pqact.gempak_decoders"'
   echo 'exec	"pqact -f WMO etc/pqact.gempak_nwx"'
   echo 'exec	"pqact -f MCIDAS|NIMAGE etc/pqact.gempak_images"'
   echo 'exec	"pqact -f NNEXRAD|WSI|FNEXRAD|EXP etc/pqact.gempak_nexrad"'
   #echo 'exec	"pqact -f WMO|CONDUIT etc/pqact.gempak_upc"'
   #echo 'exec	"pqact -f CRAFT -p BZIP2/K[A-D] etc/pqact.gempak_craft"'
   #echo 'exec	"pqact -f CRAFT -p BZIP2/K[E-K] etc/pqact.gempak_craft"'
   #echo 'exec	"pqact -f CRAFT -p BZIP2/K[L-R] etc/pqact.gempak_craft"'
   #echo 'exec	"pqact -f CRAFT -p BZIP2/K[S-Z] etc/pqact.gempak_craft"'
   echo 'exec	"pqact -f CRAFT etc/pqact.gempak_craft"'
endif
