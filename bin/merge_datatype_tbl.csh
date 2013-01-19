#!/bin/csh -f

set OLD_VERSION=GEMPAK5.9.3
set NEW_VERSION=GEMPAK5.9.4
set SOURCETAB=gempak/tables/config/datatype.tbl

set OLD=~gempak/${OLD_VERSION}/$SOURCETAB
set NEW=~gempak/${NEW_VERSION}/$SOURCETAB

awk -f ${NAWIPS}/bin/datatype.awk -v OLD=${OLD} $NEW >! /tmp/old_datatype.data
cp /tmp/old_datatype.data /tmp/new_merge.fil
awk -f ${NAWIPS}/bin/addold.awk -v OLD=/tmp/old_datatype.data $OLD >>! /tmp/new_merge.fil
rm /tmp/old_datatype.data
cat /tmp/new_merge.fil
rm /tmp/new_merge.fil

