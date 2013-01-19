#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(jbclos.o) \
	$(ALIB)(jbnext.o) \
	$(ALIB)(jbopen.o) \
	$(ALIB)(jbread.o) \
	$(ALIB)(jbwerr.o) \
	$(ALIB)(jtbinit.o) \
	$(ALIB)(jtbldsq.o) \
	$(ALIB)(jtbpdmp.o) \
	$(ALIB)(jtbqprm.o) \
	$(ALIB)(jtbsplt.o) \
	$(ALIB)(jtbtbla.o) \
	$(ALIB)(jtbtblb.o)

GDIR = jb

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<

install:

clean:
	$(RM) *.o 

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

