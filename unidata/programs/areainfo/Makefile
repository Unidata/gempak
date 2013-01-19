#
#          Makefile for areaInfo
#
# Use make [option]   where option is:
#
#          [nothing]  Makes areaInfo
#
#	   all 	      Makes areaInfo
#          areaInfo     Makes areaInfo
#
#          install:   Install INSTALLEES into their destinations.
#
#          clean:     Clean up this directory. 
#
#
# OS_BIN: Directory in which to install the areaInfo binary
# SCRIPTS_EXE: Directory to install the nsat_links script
# MANDIR: Directory in which to install the areaInfo man page
#
# Chiz		12/95	Modified areaInfo for GOES-8 & GOES-9
#			created output format for NSAT
#############################################################
include	$(CONFIGDIR)/$(MAKEINC)
#
MAN1DIR = $(MANDIR)/man1


# File-protection modes for installed files:
BINMODE	= 755

INSTALLEES	= $(OS_BIN)/areaInfo

PROGS	= areaInfo

COBJS = \
	filename.o \
	mc_tables.o \
	mc_rarea.o \
	efopen.o \
	main.o

all : areaInfo

areaInfo :  $(COBJS) 
	$(LINK.c) $(COBJS) -o $@

mc_rarea.o: mc_area.h

clean :
	rm -f core $(PROGS) $(COBJS) 

install: $(INSTALLEES) 

$(OS_BIN)/areaInfo:	areaInfo
	$(INSTALL) $? $@

$(MAN1DIR):
	mkdir -p $@

programs_gf:
