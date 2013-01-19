#
#	Makefile for nmaplib
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/nmaplib.a

FOBJS = \
	$(ALIB)(nsfdspl.o) \
	$(ALIB)(nsftlst.o)

COBJS = \
	$(ALIB)(nsfgtim.o) \
	$(ALIB)(nsfinit.o) \
	$(ALIB)(nsfplot.o) \
	$(ALIB)(nsfqatt.o) \
	$(ALIB)(nsfrest.o) \
	$(ALIB)(nsfsatt.o) \
	$(ALIB)(nsfsave.o)

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

tlink : testnsf.o 
	$(LINK.f) testnsf.o -o testnsf $(ALIB) $(OS_LIB)/sfmap.a \
	$(GEMLIB) $(CGEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) testnsf $(OS_BIN)
	$(RM) *.o testnsf 

clean:
	$(RM) *.o 
