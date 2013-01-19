#
#       Makefile for GEMPAK GRIDLIB
#               Builds the gridlib.a archive
#               Unidata 9/08 M.James
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	gd \
	gdu \
	gr \
	gu

all :
	@for dir in $(SUBDIRS); do \
		( cd $$dir; echo "Making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@ ; cd .. ) ; $(ARSLEEP) \
	done

install :

makefiles:
	griblibmake.csh

clean:
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
				*) (cd $$dir; echo -n "making clean in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ;  \
	done

