#
#	Makefile for BUFR profiler decoder, "dcprof"
#
include $(CONFIGDIR)/$(MAKEINC)

LIBS = $(BRIDGE) $(CGEMLIB) $(GEMLIB) $(LDMLOG)

PROG = dcprof

PROG_OBJS =  \
	dcprof.o \
	dcdopr.o \
	dcgbufr.o \
	decode_pr.o \
	gbytes.o \
	rd_bufr_pr.o \
	w3fi01.o \
	w3fi88.o \
	w3fc05.o

#	w3ai39.o

ALT_OBJS = w3ai39.o

CFLAGS = $(COPT) -I$(GEMPAK)/source/bridge/dc -I$(NAWIPS)/unidata/ldmlog
FFLAGS = $(FOPT) $(FOPT_NOGLOB)

all : $(PROG) 

$(PROG): $(PROG_OBJS)
	@case $(NA_OS) in \
		linux ) \
		if [ $(FC) == "g77" ]; then \
		   $(COMPILE.f) -o w3ai39.o w3ai39_linux.f ; \
		else \
		   $(COMPILE.f) w3ai39.f ; \
		fi;; \
		sol_gcc ) \
		   $(COMPILE.f) -o w3ai39.o w3ai39_linux.f ;; \
		*) \
		$(COMPILE.f) w3ai39.f ;; \
	esac
	$(LINK.f) $(NOFORMAIN) $(PROG_OBJS) $(ALT_OBJS) $(LIBS) $(SYSLIBS) -lm -o $@
	
install : $(OS_BIN)/$(PROG) $(GEMHLP)/$(PROG).hlp $(GEMTBL)/pack/profiler.pack \
	$(GEMTBL)/grid/tableb $(GEMTBL)/grid/tabled $(GEMTBL)/stns/profiler.stn

$(OS_BIN)/$(PROG) : $(PROG)
	-@cmp -s $? $@ || \
		($(INSTALL) $? $@ && echo 'updated $@')

$(GEMHLP)/$(PROG).hlp : $(PROG).hlp
	-@cmp -s $? $@ || \
		($(AUXINSTALL) $? $@ && echo 'updated $@')

$(GEMTBL)/pack/profiler.pack : profiler.pack
	-@cmp -s $? $@ || \
		($(AUXINSTALL) $? $@ && echo 'updated $@')

$(GEMTBL)/grid/tableb : tableb
	-@cmp -s $? $@ || \
		($(AUXINSTALL) $? $@ && echo 'updated $@')

$(GEMTBL)/grid/tabled : tabled
	-@cmp -s $? $@ || \
		($(AUXINSTALL) $? $@ && echo 'updated $@')

$(GEMTBL)/stns/profiler.stn : profiler.stn
	-@cmp -s $? $@ || \
		($(AUXINSTALL) $? $@ && echo 'updated $@')

clean:
	rm -f $(PROG_OBJS) $(PROG) *~ *.o 2000091917_prof.gem

reallyclean: clean
	rm -f $(PROG)

test:
	cat iupt01.bufr | dcprof -v 4 -d - tableb tabled YYYYMMDDHH_prof.gem
