#!/bin/csh

echo "Enter the OS type"
set opsys=$<

echo "Enter the type of tarfile (execs, exeonly, etc)"
set type=$<

echo "Enter the date of the tarfile"
set dat=$<

echo "Enter the password for the distb account"
set pw=$<

cd ~/deliver/tarfiles

ftp -v -n hp1 << EOF
user distb ${pw}
bin
put ${type}_${opsys}_${dat}.tar.Z
chmod 600 ${type}_${opsys}_${dat}.tar.Z
bye
EOF
