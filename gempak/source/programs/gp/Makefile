#
#	Makefile for GEMPAK 6.0 
#		Follows each limb of SF tree
#		Unidata 11/99 Chiz
#
SUBDIRS = \
	gpbox \
	gpclear \
	gpclose \
	gpcolor \
	gpend \
	gpfax \
	gpmap \
	gptcww \
	gptext \
	gptpc \
	gpfront \
	gpwarn \
	gpnexr2 \
	gpvad \
	gpnids

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
