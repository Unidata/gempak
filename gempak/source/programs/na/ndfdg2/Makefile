#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = \
	$(ALIB)(ndftim.o) \
	$(ALIB)(ndgds.o) \
	$(ALIB)(ndlamb.o)

FOBJS = \
	$(ALIB)(ndganl.o) \
	$(ALIB)(ndgcog.o) \
	$(ALIB)(ndggds.o) \
	$(ALIB)(ndginp.o) \
	$(ALIB)(ndgnav.o) \
	$(ALIB)(ndgssg.o) \
	$(ALIB)(ndgtbl.o) \
	$(ALIB)(ndparm.o)

ALIB = $(OS_LIB)/ndfdg2.a
 
PROG = ndfdg2
PROG.f = ndfdg2.f
 
LIBINC = \
	$(OS_LIB)/mdlg2dec.a \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
        $(XMLLIB) \
        $(ZLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(SYSLIB)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(CSYSLIB) -lm -o $@

programs_gf:

programs_nc: 

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

