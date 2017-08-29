#
#	Makefile for GEMPAK7
#		Follows each limb of source tree
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = libncepBUFR

all :
	@for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@ ) ; \
	done

clean:
	@for dir in $(SUBDIRS); do \
	( cd libncepBUFR; echo "making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@ ) ; \
	done
