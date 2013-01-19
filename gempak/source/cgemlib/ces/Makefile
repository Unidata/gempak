#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(CGEMLIB)

COBJS = \
	$(ALIB)(cesget.o) \
	$(ALIB)(cesgetflag.o) \
	$(ALIB)(cesgetinx.o) \
	$(ALIB)(cesgtgavid.o) \
	$(ALIB)(cesgtggrps.o) \
	$(ALIB)(cesgtgid.o) \
	$(ALIB)(cesgtginfo.o) \
	$(ALIB)(cesgtglbls.o) \
	$(ALIB)(cesgtgmgrps.o) \
	$(ALIB)(cesgtgmsid.o) \
	$(ALIB)(cesgtgnam.o) \
	$(ALIB)(cesgtlmstr.o) \
	$(ALIB)(cesgtrtbl.o) \
	$(ALIB)(cesrtbl.o) \
	$(ALIB)(cesset.o)
 
GDIR = ces

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

