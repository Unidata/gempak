#!/bin/bash

for line in `cat fix_nojs.txt`;
do 
	sed -i 's|</body>|<script src="/gempak/add_in.js" type="text/javascript"></script>\n</body>|gi' $line
done
