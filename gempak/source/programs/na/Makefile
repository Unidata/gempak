#
#	Makefile for GEMPAK 6.0 
#		Follows each limb of NA tree
#		Unidata 11/99 Chiz
#
SUBDIRS = \
	nagrib \
	nagrib2 \
	ndfdg2 \
	gd2ndfd \
	namsnd

all :
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "making in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done


programs_nc:
	@echo "Nothing to make for programs_gf in programs/na"

programs_gf:
	@echo "Nothing to make for programs_gf in programs/na"

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
