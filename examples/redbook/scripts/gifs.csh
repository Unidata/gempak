#!/bin/csh -f

if( -e /opt/myunidata ) then
   set PREF="/opt/myunidata"
else
   set PREF=""
endif

cd $PREF/content/software/gempak/examples/redbook/gifs

setenv TZ GMT

echo '<h3><pre>'
set TYPES=`ls *-*.gif* | cut -f1 -d"-" | sort -u`
foreach TYPE ($TYPES)
   set FILE=`ls -t ${TYPE}* | head -1`
   set DATE=`ls -go ${FILE} | tr -s " " | cut -f4-6 -d" "`
   set NAME=`echo $FILE:r` 
   #echo $TYPE $NAME
   echo '<a href="gifs/'${FILE}'">'${NAME}'</a> ['${DATE}']<br>'
end
echo '</pre></h3>'
