#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = \
	$(ALIB)(oabsdr.o) \
	$(ALIB)(oabstm.o) \
	$(ALIB)(oacdsp.o) \
	$(ALIB)(oacdta.o) \
	$(ALIB)(oacinp.o) \
	$(ALIB)(oacopn.o) \
	$(ALIB)(oacprm.o) \
	$(ALIB)(oacrms.o) \
	$(ALIB)(oacstn.o)

ALIB = $(OS_LIB)/oabsfc.a
 
PROG = oabsfc

LIBINC = \
	$(GPLT_NC) \
	$(GEMLIB) $(CGEMLIB) $(GPLT) $(DEVICE) \
	$(GEMLIB) $(CGEMLIB) $(GPLT) $(DEVICE) \
	$(GN) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
	$(ZLIB)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG).c
	$(COMPILE.c) $(PROG).c
	$(LINK.f) $(NOFORMAIN) $(PROG).o $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@
	$(RM) *.o

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

