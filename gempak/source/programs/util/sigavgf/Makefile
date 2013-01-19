#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/sigavgf.a

COBJS = \
	$(ALIB)(sigacld.o) \
	$(ALIB)(sigafrt.o) \
	$(ALIB)(sigajet.o) \
	$(ALIB)(sigamcld.o) \
	$(ALIB)(sigatrp.o) \
	$(ALIB)(sigatur.o) \
	$(ALIB)(sigavts.o)

FOBJS = 

PROG = sigavgf
PROG.c = sigavgf.c
 
LIBINC = \
	$(GPLT_NC) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(NC) \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB)


.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c)
	$(LINK.f) $(NOFORMAIN) $@.o $(ALIB) $(LIBINC) -lm  -o $@

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

