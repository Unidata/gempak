#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/textlib.a

COBJS = \
	$(ALIB)(bfrcld.o) \
	$(ALIB)(bfrfrt.o) \
	$(ALIB)(bfrjet.o) \
	$(ALIB)(bfrmake.o) \
	$(ALIB)(bfrmcld.o) \
	$(ALIB)(bfrrdfxy.o) \
	$(ALIB)(bfrrdmt.o) \
	$(ALIB)(bfrtrp.o) \
	$(ALIB)(bfrtur.o) \
	$(ALIB)(bfrvts.o)

GDIR = bfr

.PRECIOUS: $(ALIB)

CFLAGS = $(BUFRFLAGS) $(COPT)

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

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

