#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = \
	$(ALIB)(bdsibm.o) \
	$(ALIB)(bdsmak.o) \
	$(ALIB)(bdsnbt.o) \
	$(ALIB)(bdspgb.o) \
	$(ALIB)(bmsmak.o) \
	$(ALIB)(gbfaopn.o) \
	$(ALIB)(gbfclos.o) \
	$(ALIB)(gbfwrit.o) \
	$(ALIB)(gdgpds.o) \
	$(ALIB)(gdguin.o) \
	$(ALIB)(gdgwmo.o) \
	$(ALIB)(gdigit.o) \
	$(ALIB)(gdsced.o) \
	$(ALIB)(gdslcc.o) \
	$(ALIB)(gdsmak.o) \
	$(ALIB)(gdsmer.o) \
	$(ALIB)(gdsstr.o) \
	$(ALIB)(pdsby10.o) \
	$(ALIB)(pdsby11.o) \
	$(ALIB)(pdsbyt7.o) \
	$(ALIB)(pdsbyt9.o) \
	$(ALIB)(pdsmake.o) \
	$(ALIB)(pdsvldt.o)

LOBJS = 

ALIB = $(OS_LIB)/gdgrib.a
 
PROG = gdgrib

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB) \
	$(LIBREADLINE)

LIBINC_NC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(NC) \
	$(GN) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB) \
	$(LIBREADLINE)

PROG_NC = $(PROG)_nc

.PRECIOUS: $(ALIB)

all : gdcfil $(ALIB) $(PROG)

programs_gf:
	@echo "Not a graphic program, no _gf version needed"

programs_nc: $(PROG_NC)

$(PROG): $(ALIB) $(LIBINC) $(PROG).c
	$(COMPILE.c) $@.c
	$(LINK.f) $@.o $(ALIB) $(OS_LIB)/gdcfil.a $(LIBINC) $(SYSLIBS) -lm -o $@
	$(RM) *.o

$(PROG_NC) : $(ALIB) $(LIBINC_NC) $(PROG).c
	$(COMPILE.c) $(PROG).c
	$(LINK.f) $(PROG).o $(ALIB) $(OS_LIB)/gdcfil.a  \
		$(LIBINC_NC) $(SYSLIBS) -lm -o $(OS_BIN)/$@
	$(RM) *.o

gdcfil:
	@( cd $(GEMPAK)/source/programs/gd/gdcfil; echo "making all in `pwd`" ; \
		$(MAKE) $(MFLAGS) all )

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

