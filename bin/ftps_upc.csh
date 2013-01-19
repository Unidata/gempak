#!/bin/csh

echo "Enter the date of the tarfile"
set dat=$<

echo "Enter the password for the upc account"
set pw=$<

cd ~/deliver/tarfiles

ftp -v -n hp1 << EOF
user upc ${pw}
bin
put source_${dat}.tar.Z
chmod 600 source_${dat}.tar.Z
bye
EOF
