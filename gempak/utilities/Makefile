#
#       Makefile for GEMPAK 6.0
#               Builds the cgemlib.a archive
#               Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	a2conv \
	bounds \
	hlpbld \
	pdfbld

#	cities
#	cntcod
#	xbm

programs_gf :
	@echo "Nothing to make for programs_gf in utilities"

programs_nc :
	@echo "Nothing to make for programs_nc in utilities"

all :
	@for dir in $(SUBDIRS); do \
	   (cd $$dir; echo "making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@) ; $(ARSLEEP) \
	done

install :
	@for dir in $(SUBDIRS); do \
	   (cd $$dir; echo "making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@) \
	done

clean:
	@for dir in $(SUBDIRS); do \
	   (cd $$dir; echo "making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@) \
	done

