#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS =

LOBJS = \
	$(ALIB)(gpnarc.o) \
	$(ALIB)(gpnarw.o) \
	$(ALIB)(gpnbrb.o) \
	$(ALIB)(gpncrv.o) \
	$(ALIB)(gpnfnt.o) \
	$(ALIB)(gpninp.o) \
	$(ALIB)(gpnlin.o) \
	$(ALIB)(gpnopt.o) \
	$(ALIB)(gpnply.o) \
	$(ALIB)(gpnreg.o) \
	$(ALIB)(gpnsym.o) \
	$(ALIB)(gpntxt.o)

ALIB = $(OS_LIB)/gpanot.a
 
PROG = gpanot
PROG.f = gpanot.f
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(NETCDF) \
        $(XMLLIB) \
        $(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o $(PROG) $(ALIB)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

