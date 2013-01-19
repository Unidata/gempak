#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/textlib.a

COBJS = \
	$(ALIB)(utlampm.o) \
	$(ALIB)(utlavcd.o) \
	$(ALIB)(utlctim.o) \
	$(ALIB)(utlgdat.o) \
	$(ALIB)(utlgdwk.o) \
	$(ALIB)(utlgmon.o) \
	$(ALIB)(utlgname.o) \
	$(ALIB)(utlgtod.o) \
	$(ALIB)(utlivet.o) \
	$(ALIB)(utltomin.o) \
	$(ALIB)(utlugcp.o) \
	$(ALIB)(utlwfos.o) \
	$(ALIB)(utlwnmst.o)


 
GDIR = utl

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

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

