#
#       Makefile for GEMPAK 6.0
#               Builds the cgemlib.a archive
#               Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	upc \
	cap \
	clz \
	cgr \
	cds \
	cvq \
	ces \
	crg \
	clo \
	cvg \
	gh \
	cmd


all :
	@for dir in $(SUBDIRS); do \
	   (cd $$dir; echo "making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@) ; $(ARSLEEP) \
	done

install :
	@cd cvg; $(MAKE) $(MFLAGS) $@

makefiles:
	genmake.csh

clean:
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "making clean in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ;  \
	done 

testprog :
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "making in " ; \
				pwd ; $(MAKE) $(MFLAGS) tlink) ;; \
		esac ;  \
	done
