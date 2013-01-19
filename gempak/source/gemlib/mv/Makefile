#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(mvbtoi.o) \
	$(ALIB)(mvev32.o) \
	$(ALIB)(mvitob.o) \
	$(ALIB)(mvve32.o)

FOBJS = \
	$(ALIB)(mvei32.o) \
	$(ALIB)(mvie32.o) \
	$(ALIB)(mviv32.o) \
	$(ALIB)(mvsw42.o) \
	$(ALIB)(mvswp2.o) \
	$(ALIB)(mvswp4.o) \
	$(ALIB)(mvvi32.o)
 
GDIR = mv

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

