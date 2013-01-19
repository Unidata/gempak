#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = 

FOBJS = \
	$(ALIB)(mvev32.o) \
	$(ALIB)(mvve32.o)
 
GDIR = mv

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(FOBJS) $(COBJS)
	@for file in *.o; do make $$file ; done
$(FC) $(FFLAGS) -c $(?:.o=.f)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

tlink : test$(GDIR).f 
	$(LINK.f) test$(GDIR).f -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR) 

