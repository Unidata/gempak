#
#	Makefile for nmaplib
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/nmaplib.a

FOBJS = \
	$(ALIB)(nmsdspl.o)

COBJS = \
	$(ALIB)(nmsinit.o) \
	$(ALIB)(nmsplot.o) \
	$(ALIB)(nmsqatt.o) \
	$(ALIB)(nmsrest.o) \
	$(ALIB)(nmsrtbl.o) \
	$(ALIB)(nmssatt.o) \
	$(ALIB)(nmssave.o)

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

tlink : testnms.f
	$(LINK.f) testnms.f -o testnms $(ALIB) \
	$(GEMLIB) $(CGEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) testnms $(OS_BIN)
	$(RM) *.o testnms 

clean:
	$(RM) *.o
