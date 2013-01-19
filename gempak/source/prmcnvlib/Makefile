#
#       Makefile for GEMPAK 5.2
#               Builds the gemlib.a archive
#               Unidata 7/94 P.Bruehl
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	pc \
	pr \
	ps \
	pt

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

