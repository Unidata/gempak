#
#	Makefile for GEMPAK 6.0
#		Follows device driver source tree
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	gn \
	nc \
	ps \
	xw \
	gf \
	gif \
	xwp \
	fax \
	utf \
	vg \
	rbk \
	tiff \
	png


all :
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "$(OS) making in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ;  \
	done

makefiles:
	genmake.csh

install :
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
		NODIR) ;; \
		*) (cd $$dir; echo -n "$(OS) installing in " ; \
			pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ;  \
	done

clean:
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
		NODIR) ;; \
		*) (cd $$dir; echo -n "$(OS) making clean in " ; \
			pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ;  \
	done

