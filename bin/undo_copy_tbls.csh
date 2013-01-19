#!/bin/csh

cd $GEMTBL/nwx

echo "  "
echo "  Copying guidata.tbl to guidata.tbl.new"
echo "  "
cp guidata.tbl guidata.tbl.new

echo "  "
echo "  Copying guidata.tbl.old to guidata.tbl."
echo "  "
cp guidata.tbl.old guidata.tbl

echo "  "
echo "  Copying master.tbl to master.tbl.new"
echo "  "
cp master.tbl master.tbl.new


echo "  "
echo "  Copying master.tbl.old to master.tbl."
echo "  "
cp master.tbl.old master.tbl
echo "  "
echo "  "

