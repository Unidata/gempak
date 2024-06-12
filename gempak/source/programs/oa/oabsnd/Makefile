#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = \
	$(ALIB)(oandsp.o) \
	$(ALIB)(oandta.o) \
	$(ALIB)(oaninp.o) \
	$(ALIB)(oanopn.o) \
	$(ALIB)(oanprm.o) \
	$(ALIB)(oanrms.o) \
	$(ALIB)(oanstn.o)

ALIB = $(OS_LIB)/oabsnd.a
 
PROG = oabsnd
PROG.f = oabsnd.f
 
LIBINC = \
	$(GPLT_NC) \
	$(GEMLIB) $(CGEMLIB) $(GPLT) $(DEVICE) \
	$(GEMLIB) $(CGEMLIB) $(GPLT) $(DEVICE) \
	$(GN) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
	$(ZLIB) \
	$(LIBREADLINE)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

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

