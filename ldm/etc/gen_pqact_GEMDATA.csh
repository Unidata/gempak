#!/bin/csh -f

if (! $?GEMTBL) then
   echo set GEMTBL environmental variable before running script
   exit
endif

if (! $?GEMPAK) then
   echo set GEMPAK environmental variable before running script
   exit
endif

set RELATIVE_GEMDATA=data_spare/gempak

echo "Generating pqact.gempak file using:"
echo "   GEMTBL = $GEMTBL"
echo "   GEMPAK = $GEMPAK"
echo "   RELATIVE_GEMDATA = $RELATIVE_GEMDATA"

set NAWIPS_LDM=$NAWIPS/ldm/etc
#
# The following creates DC decoder entries with GEMTBL and GEMPAK defined
# for the local system
#
while ( ! $?PQACT )
   echo -n 'Do you want to combine entries to a single pqact.gempak? [y/n] '
   set PQACT=$<
   if ( $?PQACT ) then
      set CH = `echo $PQACT | cut -c1 | tr '[A-Z]' '[a-z]'`
      if ($CH == 'y') then
         set PQACT=1
      else if ($CH == 'n') then
         set PQACT=0
      else
         unset PQACT
      endif
   endif
end

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
   sed 's@\@GEMPAK\@@'${GEMPAK}'@g' | \
   sed 's@data/gempak@'${RELATIVE_GEMDATA}'@g' >! $DECODERS

cat $NAWIPS_LDM/templates/pqact.gempak_decoders.in | sed 's@\@GEMTBL\@@'${GEMTBL}'@g' | \
   sed 's@\@GEMPAK\@@'${GEMPAK}'@g' | \
   sed 's@data/gempak@'${RELATIVE_GEMDATA}'@g' >>! $DECODERS
#
# NWX FILE actions
#
cat $NAWIPS_LDM/templates/pqact.gempak_nwx | sed 's@data/gempak@'${RELATIVE_GEMDATA}'@g' >>! $NWX

#
# Unidata/Wisconsin Images
#
cat $NAWIPS_LDM/templates/pqact.gempak_images | sed 's@data/gempak@'${RELATIVE_GEMDATA}'@g' >>! $IMAGES

#
# NEXRAD actions
#
cat $NAWIPS_LDM/templates/pqact.gempak_nexrad | sed 's@data/gempak@'${RELATIVE_GEMDATA}'@g' >>! $NEXRAD

#
# CRAFT actions
cat $NAWIPS_LDM/templates/pqact.gempak_craft | sed 's@data/gempak@'${RELATIVE_GEMDATA}'@g' >>! $CRAFT

#
# Unidata local actions
#
#cat templates/pqact.gempak_upc | sed 's@data/gempak@'${RELATIVE_GEMDATA}'@g' >>! $UPC

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
   echo 'exec	"pqact -f NNEXRAD|WSI|FNEXRAD etc/pqact.gempak_nexrad"'
   #echo 'exec	"pqact -f WMO|CONDUIT etc/pqact.gempak_upc"'
   echo 'exec	"pqact -f CRAFT -p BZIP2/K[A-D] etc/pqact.gempak_craft"'
   echo 'exec	"pqact -f CRAFT -p BZIP2/K[E-K] etc/pqact.gempak_craft"'
   echo 'exec	"pqact -f CRAFT -p BZIP2/K[L-R] etc/pqact.gempak_craft"'
   echo 'exec	"pqact -f CRAFT -p BZIP2/K[S-Z] etc/pqact.gempak_craft"'
endif
