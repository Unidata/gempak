#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = \
	$(ALIB)(gpwinp.o) \
	$(ALIB)(gpwopt.o)

ALIB = $(OS_LIB)/gptcww.a
 
PROG = gptcww
PROG.f = gptcww.f
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(NETCDF) \
        $(XMLLIB) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2)


.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

programs_gf:
	@echo "Nothing to make for _gf"

programs_nc:
	@echo "Nothing to make for _nc"

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

$(ALIB): $(LOBJS) $(COBJS)
	-$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

