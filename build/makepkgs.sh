#!/bin/bash
cd /machine/GEMPAK7/
# create tables, extlibs, ascii maps archives
#git archive HEAD gempak/tables | gzip > gempak-tables.tar.gz
#git archive HEAD extlibs | gzip > gempak-extlibs.tar.gz
#tar -cvzf gempak-ascii.tar.gz gempak/maps/ascii
# copy to robin/conan
scp gempak-tables.tar.gz mjames@www:/content/downloads/gempak/latest/
scp gempak-extlibs.tar.gz mjames@www:/content/downloads/gempak/latest/
scp gempak-ascii.tar.gz mjames@www:/content/downloads/gempak/latest/
