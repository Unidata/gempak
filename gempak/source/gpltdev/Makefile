#
#	Makefile for GEMPAK 6.0
#		Builds gplt.a/gpltdev.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	access \
	attribute \
	color \
	control \
	plot \
	satellite \
	window 

all: 
	@for dir in $(SUBDIRS); do \
		( cd $$dir; echo "making $@ in `pwd`" ; \
			$(MAKE) $(MFLAGS) $@ ; cd .. ); $(ARSLEEP) \
	done

install:

clean:
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "making clean in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done
