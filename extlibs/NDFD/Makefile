#
#	Makefile for GEMPAK 6.0
#		Builds NDFD library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	mdlg2dec \
	mdlg2enc

all :
	@for dir in $(SUBDIRS) ; do \
	   (cd $$dir; echo "making $@ in `pwd` " ; \
		$(MAKE) $(MFLAGS) $@) ; $(ARSLEEP) \
	done

install :

clean:
	@for dir in $(SUBDIRS) ; do \
	   (cd $$dir; echo "making $@ in `pwd` " ; \
		$(MAKE) $(MFLAGS) $@) ; \
	done
