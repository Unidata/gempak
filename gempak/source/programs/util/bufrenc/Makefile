#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/bufrenc.a

COBJS =

FOBJS = \
	$(ALIB)(bfeinp.o)

PROG = bufrenc
PROG.c = bufrenc.c
 
LIBINC = \
	$(ALIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(OS_LIB)/textlib.a \
	$(OS_LIB)/libmel_bufr.a \
	$(APPL) \
	$(SYSLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(LIBREADLINE)

CFLAGS = $(COPT) $(BUFRFLAGS)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c)
	$(LINK.f) $(NOFORMAIN) $@.o $(LIBINC) $(SYSLIBS) -o $@

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

