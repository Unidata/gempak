#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	access \
	attribute \
	color \
	control \
	curve \
	draw \
	plot \
	satellite \
	window 

all:
	@for dir in $(SUBDIRS); do \
		(cd $$dir; echo "making $@ in `pwd`" ; \
			$(MAKE) $(MFLAGS) $@) ; $(ARSLEEP) \
	done

install :

clean: 
	@for dir in $(SUBDIRS); do \
		(cd $$dir; echo "making $@ in `pwd`" ; \
			$(MAKE) $(MFLAGS) $@) ; \
	done
