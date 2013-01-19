#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/dat2pts.a

COBJS = 

FOBJS = \
	$(ALIB)(d2pday.o) \
	$(ALIB)(d2pfil.o) \
	$(ALIB)(d2phdr.o) 

PROG = dat2pts
PROG.f = dat2pts.f
 

LIBINC = \
	$(ALIB) \
	$(GEMLIB) \
	$(CGEMLIB)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(LIBINC) $(PROG.f)
	$(LINK.f) $(NEED_U77) $(PROG.f) $(LIBINC) $(SYSLIBS) -o $@

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<

clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

