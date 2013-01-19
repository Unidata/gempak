#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(flapnd.o) \
	$(ALIB)(flbksp.o) \
	$(ALIB)(flcdel.o) \
	$(ALIB)(flclal.o) \
	$(ALIB)(flclos.o) \
	$(ALIB)(fldcre.o) \
	$(ALIB)(fldopn.o) \
	$(ALIB)(fldsop.o) \
	$(ALIB)(flflun.o) \
	$(ALIB)(flfpth.o) \
	$(ALIB)(flglun.o) \
	$(ALIB)(flgnam.o) \
	$(ALIB)(flinqr.o) \
	$(ALIB)(flmdat.o) \
	$(ALIB)(flmfil.o) \
	$(ALIB)(flmfls.o) \
	$(ALIB)(flmnam.o) \
	$(ALIB)(flpath.o) \
	$(ALIB)(flread.o) \
	$(ALIB)(flrewd.o) \
	$(ALIB)(flrshr.o) \
	$(ALIB)(flscnd.o) \
	$(ALIB)(flsopn.o) \
	$(ALIB)(flsunk.o) \
	$(ALIB)(flswop.o) \
	$(ALIB)(fltbop.o) \
	$(ALIB)(fltdat.o) \
	$(ALIB)(fltinq.o) \
	$(ALIB)(fltrew.o) \
	$(ALIB)(flwrit.o)
 
GDIR = fl

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS)
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
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(CGEMLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

