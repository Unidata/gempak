#
#	Makefile for GEMPAK 6.0 
#		Follows each limb of DC tree
#		Unidata 11/99 Chiz
#
SUBDIRS = \
	dcacft \
	dcairm \
	dccsig \
	dcffa \
	dcffg \
	dcgmos \
	dchrcn \
	dcidft \
	dcigdr \
	dcisig \
	dclsfc \
	dcmetr \
	dcmsfc \
	dcncon \
	dcnmos \
	dcrdf \
	dcscd \
	dcshef \
	dcsvrl \
	dctaf \
	dcxmos \
	dcuair \
	dcwarn \
	dcwcn \
	dcwcp \
	dcwou \
	dcwstm \
	dcwtch

all :
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "making in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done


programs_nc:
	@echo "Nothing to make for programs_nc in decoders"

programs_gf:
	@echo "Nothing to make for programs_nc in decoders"

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
