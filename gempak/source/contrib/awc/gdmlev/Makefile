#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS =

LOBJS = \
	$(ALIB)(gdmaxs.o) \
	$(ALIB)(gdmglv.o) \
	$(ALIB)(gdminp.o) \
	$(ALIB)(gdmplt2.o)

ALIB = $(OS_LIB)/gdmlev.a
 
PROG = gdmlev
PROG.f = gdmlev.f
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(XMLLIB) \
	$(OS_LIB)/gdmap.a \
	$(GPLT) \
	$(GEMLIB) \
	$(CGEMLIB) \
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

