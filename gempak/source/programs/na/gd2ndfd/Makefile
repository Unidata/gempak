#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = \
	$(ALIB)(gbfaopn.o) \
	$(ALIB)(gbfclos.o) \
	$(ALIB)(gbfwrit.o)

FOBJS = \
	$(ALIB)(ndfdgds.o) \
	$(ALIB)(ndfdinp.o) \
	$(ALIB)(ndfdlcc.o) \
	$(ALIB)(ndfdpds.o) \
	$(ALIB)(gdgwmo.o) \
	$(ALIB)(gdigit.o) \
	$(ALIB)(gds_ced.o) \
	$(ALIB)(gds_mer.o) \
	$(ALIB)(gds_str.o)

ALIB = $(OS_LIB)/gd2ndfd.a
 
PROG = gd2ndfd
PROG.f = gd2ndfd.f
 
LIBINC = \
	$(OS_LIB)/gdgrib.a \
	$(OS_LIB)/mdlg2enc.a \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
	$(ZLIB) \
	$(CGEMLIB)

.PRECIOUS: $(ALIB)

all : gdgrib gdcfil $(ALIB) $(PROG)

gdcfil:
	@( cd $(GEMPAK)/source/programs/gd/gdcfil; echo "making all in `pwd`" ; \
		$(MAKE) $(MFLAGS) all )

gdgrib:
	@( cd $(GEMPAK)/source/programs/gd/gdgrib; echo "making all in `pwd`" ; \
		$(MAKE) $(MFLAGS) all )

$(PROG): $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

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

