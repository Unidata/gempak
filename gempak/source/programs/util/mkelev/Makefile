#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/mkelev.a

COBJS = 

FOBJS = $(ALIB)(ccplot.o)

PROG = mkelev
PROG.f = mkelev.f
 

LIBINC = \
	$(ALIB) \
        $(OS_LIB)/gdcntr.a \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
        $(SYSLIB) \
        $(NETCDF) \
        $(LIBJASPER) \
        $(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(GEMLIB) \
        $(CGEMLIB) \
        $(RSLLIB) \
        $(LIBBZ2)


.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(LIBINC) $(PROG.f)
	$(LINK.f) $(NEED_U77) $(PROG.f) $(LIBINC) $(SYSLIBS) -o $@

$(ALIB): $(FOBJS) $(COBJS)
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

