#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	airmet \
	bfr \
	cas \
	uka \
	utl \
	wbc \
	xml

OSDIRS = airmet

all :
	@for dir in $(SUBDIRS) ; do \
	   (cd $$dir; echo "making $@ in `pwd` " ; \
		$(MAKE) $(MFLAGS) $@) ; $(ARSLEEP) \
	done

install :
	@case $(OPSYS) in \
	  LINUX ) \
	     (cd $(OSDIRS); echo "$(OPSYS) - making $@ in `pwd` " ; \
		$(MAKE) $(MFLAGS) $@) ;; \
	esac

clean:
	@for dir in $(SUBDIRS) ; do \
	   (cd $$dir; echo "making $@ in `pwd` " ; \
		$(MAKE) $(MFLAGS) $@) ; \
	done
