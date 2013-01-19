#
#	Makefile for GEMPAK 6.0
#		Builds appl.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	access \
	attribute \
	bounds \
	color \
	control \
	curve \
	graph \
	grid \
	map \
	plot \
	satellite \
	transform \
	utility 

all : 
	@for dir in $(SUBDIRS); do \
		(cd $$dir; echo "making $@ in `pwd`" ; \
			$(MAKE) $(MFLAGS) $@) ; $(ARSLEEP) \
	done

install :

makefiles:
	genmake.csh

clean:
	@for dir in $(SUBDIRS); do \
		(cd $$dir; echo "making $@ in `pwd`" ; \
			$(MAKE) $(MFLAGS) $@) ; \
	done
