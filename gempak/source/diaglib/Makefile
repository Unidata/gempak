#
#       Makefile for GEMPAK DIAGLIB
#               Builds the diaglib.a archive
#               Unidata 9/08 M.James
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	de \
	df \
	dg \
	dl \
	dv \
	grc \
	pd \
	cgd

all :
	@for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@ ; cd .. ) ; $(ARSLEEP) \
	done

install :

makefiles:
	diagmake.csh

clean:
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "making clean in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ;  \
	done

