#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

LIBINC =

COBJS = \
	$(ALIB)(poly_clipw.o) \
	$(ALIB)(poly_cvgfw.o)

LOBJS = \
	$(ALIB)(poly_apad.o) \
	$(ALIB)(poly_cap.o) \
	$(ALIB)(poly_ccwa.o) \
	$(ALIB)(poly_driv.o) \
	$(ALIB)(poly_exist.o) \
	$(ALIB)(poly_group.o) \
	$(ALIB)(poly_init.o) \
	$(ALIB)(poly_ltln.o) \
	$(ALIB)(poly_margin.o) \
	$(ALIB)(poly_opnf.o) \
	$(ALIB)(poly_pad.o) \
	$(ALIB)(poly_rdparm.o) \
	$(ALIB)(poly_rmflg.o) \
	$(ALIB)(poly_szone.o) \
	$(ALIB)(poly_warn.o)

ALIB = $(OS_LIB)/gpolyg.a
 
PROG = gpolyg
PROG.f = gpolyg.f
 

LIBINC = \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(OS_LIB)/libgpc.a \
	$(NETCDF) \
	$(OS_LIB)/rsl.a \
        $(XMLLIB) \
	$(ZLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(LIBBZ2)

LIBINC_NC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(NC) \
	$(GN) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(OS_LIB)/libgpc.a \
        $(NETCDF) \
        $(OS_LIB)/rsl.a \
        $(XMLLIB) \
        $(ZLIB) \
        $(GEMLIB) \
	$(CGEMLIB) \
        $(LIBBZ2)

LIBINC_GF = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GF) \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(OS_LIB)/libgpc.a \
        $(NETCDF) \
        $(OS_LIB)/rsl.a \
        $(XMLLIB) \
        $(ZLIB) \
        $(GEMLIB) \
	$(CGEMLIB) \
        $(LIBBZ2)

PROG_NC = $(PROG)_nc
PROG_GF = $(PROG)_gf

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -o $@

$(PROG_NC) : $(ALIB) $(LIBINC_NC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_NC) $(SYSLIBS) -o $(OS_BIN)/$@

$(PROG_GF) : $(ALIB) $(LIBINC_GF) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GF) \
		$(X11LIBDIR) $(X11LIB) $(SYSLIBS) $(SYSLIBS) -o $(OS_BIN)/$@

programs_gf: $(PROG_GF)

programs_nc: $(PROG_NC)

$(ALIB): $(LOBJS) $(COBJS)
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

