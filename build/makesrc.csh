#!/bin/csh
# get current GEMPAK version number
source /machine/GEMPAK7/Gemenviron
set VERSION=`grep "vmsg =" /machine/GEMPAK7/gempak/source/gemlib/ss/ssvers.f | cut -d " " -f4 | sed "s/'//"`

cd /machine/rpmbuild/SPECS/
rm -rf master.tar.gz

# download latest source from github
wget --no-check-certificate https://github.com/Unidata/gempak/archive/master.tar.gz
tar -xvzf master.tar.gz
mv gempak-master GEMPAK7
tar -cvzf gempak-${VERSION}.tar.gz GEMPAK7
rm -rf gempak-master GEMPAK7

# copy to web server
scp gempak-${VERSION}.tar.gz www:/content/downloads/gempak/latest/

# update table of contents with latest source version
sed 's/GEMPAK_VERSION/'${VERSION}'/' toc-template.xml > toc.xml
scp toc.xml www:/content/downloads/gempak/latest/


# tables, maps, extlibs
cd /machine/GEMAPK7/
git archive HEAD gempak/tables | gzip > gempak-tables.tar.gz
git archive HEAD extlibs | gzip > gempak-extlibs.tar.gz
tar -cvzf gempak-ascii.tar.gz gempak/maps/ascii
# copy to robin/conan
scp gempak-tables.tar.gz www:/content/downloads/gempak/latest/
scp gempak-extlibs.tar.gz www:/content/downloads/gempak/latest/
scp gempak-ascii.tar.gz www:/content/downloads/gempak/latest/
