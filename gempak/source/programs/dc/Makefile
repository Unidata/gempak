#
#	Makefile for GEMPAK 6.0 
#		Follows each limb of DC tree
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	dcacars \
	dcacft \
	dcairm \
	dccosmic \
	dccsig \
	dcffa \
	dcffg \
	dcgmos \
	dcgrib \
	dcgrib2 \
	dchrcn \
	dcidft \
	dcigdr \
	dcisig \
	dclsfc \
	dcmetr \
	dcmsfc \
	dcncon \
	dcncprof \
	dcnexr2 \
	dcnldn \
	dcnmos \
	dcprof \
	dcrdf \
	dcreanal \
	dcreanal2 \
	dcredbook \
	dcscd \
	dcshef \
	dcsuomi \
	dcstorm \
	dcsvrl \
	dctaf \
        dctama \
	dctrop \
	dcxmos \
	dcuair \
	dcuspln \
	dcwarn \
	dcwatch \
	dcwcn \
	dcwcp \
	dcwou \
	dcwstm \
	dcwmostrip \
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
