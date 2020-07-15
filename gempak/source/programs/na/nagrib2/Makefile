#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = \
	$(ALIB)(nadiag.o) \
	$(ALIB)(nag2in.o)

LOBJS =

ALIB = $(OS_LIB)/nagrib2.a
 
PROG = nagrib2
PROG.c = nagrib2.c
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
        $(XMLLIB) \
        $(ZLIB) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(LIBREADLINE)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c)
	$(LINK.f) $(NOFORMAIN) $(PROG).o $(ALIB) $(LIBINC) $(LIBJASPER) -lm $(CSYSLIB) $(PNGLIB) $(ZLIB) -o $@

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

