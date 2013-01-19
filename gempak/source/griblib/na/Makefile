#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(naganl.o) \
	$(ALIB)(nagcog.o) \
	$(ALIB)(naggds.o) \
	$(ALIB)(nagnav.o) \
	$(ALIB)(nagssg.o) \
	$(ALIB)(nagtbl.o) \
	$(ALIB)(nainit.o) \
	$(ALIB)(nalevl.o) \
	$(ALIB)(narhdr.o) \
	$(ALIB)(narprm.o) \
	$(ALIB)(nartbl.o) \
	$(ALIB)(nasmry.o)

FOBJS = 
 
GDIR = na

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

