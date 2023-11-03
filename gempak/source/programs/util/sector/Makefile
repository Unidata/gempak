#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS =

LOBJS = \
	$(ALIB)(secimg.o) \
	$(ALIB)(secinp.o)
 
ALIB = $(OS_LIB)/sector.a
 
PROG = sector
PROG.f = sector.f
 

LIBINC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(ZLIB) \
	$(LIBREADLINE)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) -o $@

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

