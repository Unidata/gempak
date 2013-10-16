#
#	Makefile for GEMPAK 6.0 
#		Follows each limb of util tree
#		Unidata 11/99 Chiz
#
SUBDIRS = \
	aodt \
	arinfo \
	bnd2shp \
	bufrenc \
	clipvgf \
	createccfp \
	createbinfo \
	dat2pts \
	datetime \
	dumpvgf \
	fop \
	rfop \
	freeColors \
	joinvgf \
	maps \
	mdp \
	mdpspc \
	mkelev \
	ndinfo \
	nesdis \
	nodt \
	prob2cat \
	rebun \
	sector \
	sigavgf \
	sigbufr \
	spenes \
	spctxt \
	tcacancel \
	tpc2shp \
	tran2fos \
	vgf2tag \
	vgftoascii \
	vgftobnd \
	wmostrip \
	woupdt \
	atest \
	plotsyms \
	fips \
	areainfo \
	nmap_kiosk \
	cwordsh \
	etamap 


all :
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "making in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done


programs_nc:
	@echo "Nothing to make for programs_nc in programs/util"

programs_gf:
	@echo "Nothing to make for programs_gf in programs/util"

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
