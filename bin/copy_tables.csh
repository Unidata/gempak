#!/bin/csh

cd $GEMTBL/nwx

echo "  "
echo "  Copying guidata.tbl to guidata.tbl.old."
echo "  "
cp guidata.tbl guidata.tbl.old

echo "  "
echo "  Copying master.tbl to master.tbl.old."
echo "  "
cp master.tbl master.tbl.old

echo "  "
echo "  Copying guidata.tbl.new to guidata.tbl"
echo "  "
cp guidata.tbl.new guidata.tbl

echo "  "
echo "  Copying master.tbl.new to master.tbl"
echo "  "
cp master.tbl.new master.tbl
echo "  "
echo "  "


