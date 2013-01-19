#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(CGEMLIB)

COBJS = \
	$(ALIB)(cmdobclear.o) \
	$(ALIB)(cmdobgetbb.o) \
	$(ALIB)(cmdobgetcntr.o) \
	$(ALIB)(cmdobgetcntrd.o) \
	$(ALIB)(cmdobgetid.o) \
	$(ALIB)(cmdobgetpoints.o) \
	$(ALIB)(cmdobispoly.o) \
	$(ALIB)(cmdobisvisible.o) \
	$(ALIB)(cmdosaddob.o) \
	$(ALIB)(cmdosclear.o) \
	$(ALIB)(cmdosdel.o) \
	$(ALIB)(cmdosdelob.o) \
	$(ALIB)(cmdosfindob.o) \
	$(ALIB)(cmdosgetob.o) \
	$(ALIB)(cmdositerinit.o) \
	$(ALIB)(cmdositernext.o) \
	$(ALIB)(cmdosnew.o) \
	$(ALIB)(cmdosnewob.o) \
	$(ALIB)(cmdossetarea.o)
 
GDIR = cmd

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
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(CGEMLIB) $(GEMLIB) $(CGEMLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

