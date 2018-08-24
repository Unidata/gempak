#
#	Makefile for GEMPAK 6.0
#		Builds gplt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GPLT)

LIBINC =

COBJS =

LOBJS = \
	$(ALIB)(asspl2.o) \
	$(ALIB)(gms5_nav.o) \
	$(ALIB)(mcnav.o) \
	$(ALIB)(nvxeas.o) \
	$(ALIB)(nvxgmsx.o) \
	$(ALIB)(nvxgoes.o) \
	$(ALIB)(nvxgoes4.o) \
	$(ALIB)(nvxgvar.o) \
	$(ALIB)(nvxabi.o) \
	$(ALIB)(nvxmoll.o) \
	$(ALIB)(nvxrect.o) \
	$(ALIB)(nvxmsat.o) \
	$(ALIB)(nvxmsg.o) \
	$(ALIB)(nvxradr.o) \
	$(ALIB)(nvxsae.o)

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

