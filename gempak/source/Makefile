#
#	Makefile for GEMPAK 6.0
#		Follows each limb of source tree
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)
NXMLIB = nxmlib

SUBDIRS = \
	appl \
	ncepUT \
	syslib \
	device \
	textlib \
	gemlib \
	awlib \
	diaglib \
	gridlib \
	griblib \
	jwblib \
	oalib \
	prmcnvlib \
	sflib \
	snlib \
	vflib \
	cgemlib \
	gpltdev \
	gplt \
	bridge \
	driver \
	nmaplib \
	programs \
	contrib


all :
	@case $(HAVEMOTIF) in \
	NO) \
	for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "making in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done \
	;;\
	*) \
	for dir in NODIR $(NXMLIB) $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo "Making $@ in `pwd`" ; \
				$(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done \
	;; \
	esac

programs_nc : 
	@cd programs; echo -n "making programs_nc in "; \
	   pwd; $(MAKE) $(MFLAGS) $@

programs_gf : 
	@cd programs; echo -n "making programs_gf in "; \
	   pwd; $(MAKE) $(MFLAGS) $@

install:
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "installing in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done

clean: 
	@for dir in NODIR $(NXMLIB) $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
		*) (cd $$dir; echo -n "making clean in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done

testprog :
	@cd gemlib ;  echo -n "making test programs in " ; \
	   pwd; $(MAKE) $(MFLAGS) $@

