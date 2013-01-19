#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(CGEMLIB)

COBJS = \
	$(ALIB)(casclos.o) \
	$(ALIB)(casopen.o) \
	$(ALIB)(casrdcld.o) \
	$(ALIB)(casrdfrt.o) \
	$(ALIB)(casrdhdr.o) \
	$(ALIB)(casrdjets.o) \
	$(ALIB)(casrdmcld.o) \
	$(ALIB)(casrdstm.o) \
	$(ALIB)(casrdtrop.o) \
	$(ALIB)(casrdturb.o) \
	$(ALIB)(casrdvlrd.o) \
	$(ALIB)(caswrcld.o) \
	$(ALIB)(caswrfrt.o) \
	$(ALIB)(caswrjets.o) \
	$(ALIB)(caswrmcld.o) \
	$(ALIB)(caswrmhdr.o) \
	$(ALIB)(caswrstm.o) \
	$(ALIB)(caswrtrop.o) \
	$(ALIB)(caswrturb.o) \
	$(ALIB)(caswrvlrd.o)
 
GDIR = cas

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

tlink : test$(GDIR).c
	$(COMPILE.c) test$(GDIR).c
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(CGEMLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

