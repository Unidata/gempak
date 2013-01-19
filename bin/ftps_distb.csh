#!/bin/csh

echo "Enter the date of the tarfile"
set dat=$<

echo "Enter the password for the distb account"
set pw=$<

cd ~/deliver/tarfiles

ftp -v -n hp1 << EOF
user distb ${pw}
bin
put source_${dat}.tar.Z
chmod 600 source_${dat}.tar.Z
bye
EOF
