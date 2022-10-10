#!/bin/bash

for line in `cat fix_wx.txt`;
do 
	sed -i 's|</html>|<script src="/gempak/add_in.js" type="text/javascript"></script>\n</body></html>|gi' $line
done
