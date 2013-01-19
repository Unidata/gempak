#
#	Makefile for GEMPAK 6.0
#		Builds nmaplib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	ngd \
	nim \
	nmp \
	nms \
	nsf \
	nsn

all : 
	@for dir in $(SUBDIRS) ; do \
	   (cd $$dir; echo "making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@) ; $(ARSLEEP) \
	done

install :

clean: 
	@for dir in $(SUBDIRS); do \
	   (cd $$dir; echo "making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@) ; \
	done
