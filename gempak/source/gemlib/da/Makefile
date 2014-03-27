#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB) 

CFLAGS += -I$(OS_INC)/libxml2

COBJS = \
        $(ALIB)(dagetdtc.o) \
        $(ALIB)(dagetdti.o) \
        $(ALIB)(dagetdtr.o) \
        $(ALIB)(dagetdtrgd.o) \
        $(ALIB)(dagetfilhdc.o) \
        $(ALIB)(dagetfilhdi.o) \
        $(ALIB)(dagetfilhdr.o) \
        $(ALIB)(dagetheader.o) \
        $(ALIB)(dagetinfo.o) \
        $(ALIB)(dareadxml.o) \
        $(ALIB)(darunpy.o)

FOBJS = \
	$(ALIB)(dainfo.o) \
	$(ALIB)(daopen.o) \
	$(ALIB)(dardtc.o) \
	$(ALIB)(dardti.o) \
	$(ALIB)(dardtr.o) \
	$(ALIB)(dardtrgd.o) \
	$(ALIB)(darfhc.o) \
	$(ALIB)(darfhi.o) \
	$(ALIB)(darfhr.o) 

GDIR = da

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
