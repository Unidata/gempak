#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = \
	$(ALIB)(nagcut.o) \
	$(ALIB)(nagfil.o) \
	$(ALIB)(nagflh.o) \
	$(ALIB)(nagflv.o) \
	$(ALIB)(naginp.o)

LOBJS =

ALIB = $(OS_LIB)/nagrib.a
 
PROG = nagrib
PROG.c = nagrib.c
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
        $(XMLLIB) \
        $(ZLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(SYSLIB) \
	$(LIBREADLINE)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(COMPILE.c) $(PROG.c)
	$(LINK.f) $(PROG).o $(ALIB) $(LIBINC) -lm $(CSYSLIB) -o $@
	$(RM) *.o

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

