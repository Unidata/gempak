#
#	Makefile for nmaplib
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/nmaplib.a

FOBJS = \
	$(ALIB)(nsndspl.o) \
	$(ALIB)(nsntlst.o)

COBJS = \
	$(ALIB)(nsngtim.o) \
	$(ALIB)(nsninit.o) \
	$(ALIB)(nsnplot.o) \
	$(ALIB)(nsnqatt.o) \
	$(ALIB)(nsnrest.o) \
	$(ALIB)(nsnsatt.o) \
	$(ALIB)(nsnsave.o)

.PRECIOUS: $(ALIB)

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC) 

all : $(ALIB)

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o


.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


install: 

tlink : testnsn.o
	$(LINK.f) testnsn.o -o testnsn $(ALIB) $(OS_LIB)/sfmap.a \
	$(GEMLIB) $(CGEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) testnsn $(OS_BIN)
	$(RM) *.o testnsn

clean:
	$(RM) *.o
