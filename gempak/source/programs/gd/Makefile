#
#	Makefile for GEMPAK 6.0 
#		Follows each limb of GD tree
#		Unidata 11/99 Chiz
#
SUBDIRS = \
	gdcfil \
	gdcntr \
	gdcross \
	gddelt \
	gddiag \
	gdedit \
	gdfrzl \
	goftxt \
	gdgrib \
	gdgrib2 \
	gdgsfc \
	gdinfo \
	gdlist \
	gdmap \
	gdmod \
	gdomeg \
	gdplot \
	gdprof \
	gdptpdf \
	gdpvsf \
	gdstat \
	gdstream \
	gdthgt \
	gdtser \
	gdtspdf \
	gdvint \
	gdwind \
	grphgd \
	gdplot2 \
	gdplot3 \
	gpolyg

all :
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "making in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done


programs_nc:
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "making programs_nc in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done

programs_gf:
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "making programs_gf in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done

install:
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "installing in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done

clean:
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
		*) (cd $$dir; echo -n "making clean in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done

makefiles:
	genmake.csh
