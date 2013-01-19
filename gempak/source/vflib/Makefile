#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(vfampm.o) \
	$(ALIB)(vfarea.o) \
	$(ALIB)(vfasaw.o) \
	$(ALIB)(vfavcd.o) \
	$(ALIB)(vfcnsaw.o) \
	$(ALIB)(vfcnsel.o) \
	$(ALIB)(vfctim.o) \
	$(ALIB)(vfgdat.o) \
	$(ALIB)(vfgname.o) \
	$(ALIB)(vfgtod.o) \
	$(ALIB)(vfgttxt.o) \
	$(ALIB)(vfrdrp.o) \
	$(ALIB)(vfread.o) \
	$(ALIB)(vfrptxt.o) \
	$(ALIB)(vfsort.o) \
	$(ALIB)(vfspc.o) \
	$(ALIB)(vfstate.o) \
	$(ALIB)(vfsval.o) \
	$(ALIB)(vftomin.o) \
	$(ALIB)(vfvors.o) \
	$(ALIB)(vfwnmst.o) \
	$(ALIB)(vfwoui.o) \
	$(ALIB)(vfwrep.o) \
	$(ALIB)(vfwsaw.o) \
	$(ALIB)(vfwsel.o) \
	$(ALIB)(vfwsev.o) \
	$(ALIB)(vfwwcl.o) \
	$(ALIB)(vfwwcp.o)
 
GDIR = vf

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<

install:

clean:
	$(RM) *.o 

tlink : test$(GDIR).c
	$(COMPILE.c) test$(GDIR).c
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(CGEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

