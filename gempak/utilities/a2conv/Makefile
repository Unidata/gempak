#
# This Make file is to install A2 converter in the $GEMPAK/utilities/a2conv
# and create the latest library file libVgfXml.so 
# To do this, make sure you have the latest awips2-converter-installer.tgz download and unzipped,
# and run this Makefile in this directory
#

all: cpBin mvLib 

cpBin: ${GEMPAK}/utilities/a2conv
	cp ${GEMPAK}/utilities/a2conv/bin/* ${OS_BIN}/

mvLib: ${GEMPAK}/utilities/a2conv/buildfile.sh 
	sh ${GEMPAK}/utilities/a2conv/buildfile.sh
