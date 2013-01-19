#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

LOBJS = \
	$(ALIB)(tbastn.o) \
	$(ALIB)(tbfgeo.o) \
	$(ALIB)(tbfont.o) \
	$(ALIB)(tbgrnv.o) \
	$(ALIB)(tbidst.o) \
	$(ALIB)(tbnids.o) \
	$(ALIB)(tbnidsdb.o) \
	$(ALIB)(tbparm.o) \
	$(ALIB)(tbpcnv.o) \
	$(ALIB)(tbprmt.o) \
	$(ALIB)(tbrstn.o) \
	$(ALIB)(tbustn.o)
 
GDIR = tb

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(LOBJS)
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

