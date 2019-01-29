#!/bin/bash -f
export GEMHLP=/Users/mjames/gempak/gempak/help
export GEMPTXT=/Users/mjames/gempak/gempak/txt/programs
DOCDIR="$( cd "$(dirname "$0")" ; pwd -P )"

# Program Pages
for prog in $(ls $GEMPTXT | cut -d . -f1); do
   progu=$(echo $prog | tr '[:lower:]' '[:upper:]')
   OUTFIL=$DOCDIR/programs/$prog.md
   echo '# '$progu > $OUTFIL
   echo '' >> $OUTFIL
   cat $GEMHLP/hlp/$prog.hlp >> $OUTFIL
done

cd $DOCDIR/programs/
git grep -l "INPUT PARAMETERS"  | xargs sed -i '' -e 's/INPUT PARAMETERS/### Input Parameters/'
git grep -l "PROGRAM DESCRIPTION"  | xargs sed -i '' -e 's/PROGRAM DESCRIPTION/### Program Description/'
git grep -l "EXAMPLES"  | xargs sed -i '' -e 's/EXAMPLES/### Examples/'
git grep -l "ERROR MESSAGES"  | xargs sed -i '' -e 's/ERROR MESSAGES/### Error Messages/'


exit

# Programs
OUTFIL=$DOCDIR/programs.md
echo '# GEMPAK Programs' > $OUTFIL
echo '' >> $OUTFIL
for prog in $(ls $GEMPTXT | cut -d . -f1); do
   pos=$(grep -n "INPUT PARAMETERS" $GEMPTXT/$prog.txt| cut -d : -f1)
   let pos=$pos-1
   regex=$(echo $prog | tr '[:lower:]' '[:upper:]')
   #cat $GEMPTXT/$prog.txt |head -4
   blurb=$(cat $GEMPTXT/$prog.txt |head -${pos} | sed 's/'$regex'//g')
   echo '* ['$prog'](programs/'$prog')'$blurb >> $OUTFIL
done

exit

# Decoder Index File
OUTFIL=$DOCDIR/decoders.md
echo '# GEMPAK Decoders' > $OUTFIL
echo '' >> $OUTFIL
for prog in $(ls $GEMHLP/hlp/ | sed 's,\.hlp$,,' | grep '^dc' | sort | uniq); do
   regex=$(echo $prog | tr '[:lower:]' '[:upper:]')

   blurb=$(cat $GEMHLP/hlp/$prog.hlp | sed 's/'$regex'//g')
   blurb=$(cat $GEMHLP/hlp/$prog.hlp | head -20 | tail -17 | sed 's/'$regex'//g')
   echo '* ['$prog'](decoders/'$prog')'$blurb >> $OUTFIL
done

# Parameters
OUTFIL=$DOCDIR/parameters.md
echo '# GEMPAK Parameters' > $OUTFIL
echo '' >> $OUTFIL
for parm in $(ls $GEMHLP/hlx | cut -d . -f 1| uniq); do
    regex=$(echo $parm | tr '[:lower:]' '[:upper:]')
    blurb=$(cat $GEMHLP/hlx/$parm.hl2 | head -20 | sed 's/'$regex'//g')
    echo '* ['$regex'](parameters/'$parm')'$blurb >> $OUTFIL
done

# NPROGS
#OUTFIL=$DOCDIR/nprogs.md
#echo '' > $OUTFIL
#echo '' >> $OUTFIL
#echo '<\!doctype html><head><title>GEMPAK Manual: GUI Programs</title></head><body>' > $OUTFIL
#echo '<a href=../../help_and_documentation>GEMPAK Manual</a> | GUI Programs (NPROGs) <br><hr>' >> $OUTFIL
#cd $GEMHLP/hlp
#for prog in $(ls *.hlp | sed 's,\.hlp$,,' | grep 'Index$' | sed 's@Index$@@' | sort | uniq); do
#   echo '<li><a href='$prog'.html>'$prog'</a></li>' >> $OUTFIL
#done
