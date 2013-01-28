#
#	Makefile for GEMPAK 6.0
#		Follows each limb of source tree
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	v64/odtlib \
	v64/odtapi \
	v64/inc \
	v72/odtlib \
	v72/odtapi \
	v72/inc

all : INCDIR
	@for dir in $(SUBDIRS); do \
	   ( cd $$dir; pwd; $(MAKE) $(MFLAGS) $@ ) ; \
	done

install:
	@for dir in $(SUBDIRS); do \
	   (cd $$dir; pwd; $(MAKE) $(MFLAGS) $@ ) ; \
	done

clean:
	@for dir in $(SUBDIRS); do \
	   (cd $$dir; pwd; $(MAKE) $(MFLAGS) $@ ) ; \
	done

programs_nc:

programs_gf:

INCDIR:
	@if [ ! -d $(OS_INC)/AODT ]; then mkdir -p $(OS_INC)/AODT; fi

