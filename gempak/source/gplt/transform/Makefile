#
#	Makefile for GEMPAK 6.0
#		Builds gplt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GPLT)

FOBJS = \
	$(ALIB)(gazdrm.o) \
	$(ALIB)(gazmlm.o) \
	$(ALIB)(gazmml.o) \
	$(ALIB)(gconlm.o) \
	$(ALIB)(gconml.o) \
	$(ALIB)(gcyllm.o) \
	$(ALIB)(gcylml.o) \
	$(ALIB)(gdrazm.o) \
	$(ALIB)(ggrflm.o) \
	$(ALIB)(ggrfml.o) \
	$(ALIB)(ggtoi.o) \
	$(ALIB)(gitog.o) \
	$(ALIB)(gitoq.o) \
	$(ALIB)(gltou.o) \
	$(ALIB)(gmerlm.o) \
	$(ALIB)(gmerml.o) \
	$(ALIB)(gmtoq.o) \
	$(ALIB)(gmtow.o) \
	$(ALIB)(gmtowq.o) \
	$(ALIB)(goeslm.o) \
	$(ALIB)(goesml.o) \
	$(ALIB)(gptvis.o) \
	$(ALIB)(gqtoi.o) \
	$(ALIB)(gqtom.o) \
	$(ALIB)(grottl.o) \
	$(ALIB)(gtrans.o) \
	$(ALIB)(gutol.o) \
	$(ALIB)(gutow.o) \
	$(ALIB)(gwtom.o) \
	$(ALIB)(gwtou.o) \
	$(ALIB)(intdlm.o) \
	$(ALIB)(rttoxy.o) \
	$(ALIB)(xytort.o)

SUBS = \
	mcidas

.PRECIOUS: $(ALIB)


all : $(ALIB)
	@for dir in NODIR $(SUBS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "making in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<



clean:
	$(RM) *.o 
	@for dir in NODIR $(SUBS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "making in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done

