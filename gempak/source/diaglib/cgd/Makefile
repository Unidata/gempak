#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(cgdglev.o) \
	$(ALIB)(cgdgnum.o) \
	$(ALIB)(cgdgtim.o) \
	$(ALIB)(cgdngrd.o) \
	$(ALIB)(cgdrdat.o) \
	$(ALIB)(cgdwpgd.o) \
	$(ALIB)(cgdwppg.o) 

GDIR = cgd

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

clean:
	$(RM) *.o

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(CGEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)
 
