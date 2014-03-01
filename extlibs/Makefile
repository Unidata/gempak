#
#	Makefile for GEMPAK7
#		Follows each limb of source tree
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	zlib \
	PNG \
	bzip2 \
	HDF5 \
	GPC \
	AODT \
	melBUFR \
	ncepBUFR \
	netCDF \
	OHSHEF \
	xml2 \
	xslt \
	JasPer \
	NDFD \
	SHAPE \
	GAMET \
	rsl

SUBDIRS_sol = \
        zlib \
        PNG \
        bzip2 \
        HDF5 \
        GPC \
        AODT \
        melBUFR \
        ncepBUFR \
        netCDF \
        OHSHEF \
        JasPer \
        NDFD \
        SHAPE \
        GAMET \
        rsl

all : $(EXTLIBMK)

extliball : 
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "making in " ; \
				pwd ; $(MAKE) $(MFLAGS) all) ;; \
		esac ; \
	done

sol :
	@for dir in NODIR $(SUBDIRS_sol); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "making in " ; \
				pwd ; $(MAKE) $(MFLAGS) all) ;; \
		esac ; \
	done

programs_nc : 

programs_gf : 

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

