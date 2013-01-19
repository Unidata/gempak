include $(CONFIGDIR)/$(MAKEINC)

PROG = nsharp

ALIB = $(OS_LIB)/nsharp.a

GLIBS = \
	local_Nxmlib.a \
	$(GPLT_NC) \
	$(OS_LIB)/snlist.a \
	$(OS_LIB)/sflist.a \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(XWP) \
	$(XW) \
	$(PS) \
	$(GN) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
	$(ZLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2)


FOBJS = \
	$(ALIB)(gdpgts.o) \
	$(ALIB)(get_file_alias.o) \
	$(ALIB)(get_gem_stns.o) \
	$(ALIB)(gdudta.o) \
	$(ALIB)(get_gem_times.o) \
	$(ALIB)(get_acars_times.o) \
	$(ALIB)(get_pfc_snd.o) \
	$(ALIB)(mapmark.o) \
	$(ALIB)(get_gem_snd.o) \
	$(ALIB)(get_mdl_snd.o) \
	$(ALIB)(mapdraw.o) \
	$(ALIB)(sndata.o)

COBJS = \
	$(ALIB)(acars_selection.o) \
	$(ALIB)(acars_status.o) \
	$(ALIB)(basics.o) \
	$(ALIB)(decoder.o) \
	$(ALIB)(draw.o) \
	$(ALIB)(file_browse_popup.o) \
	$(ALIB)(file_selection_menu.o) \
	$(ALIB)(ps_driv.o) \
	$(ALIB)(mapw.o) \
	$(ALIB)(mod_snd.o) \
	$(ALIB)(obs_snd.o) \
	$(ALIB)(pfc_snd.o) \
	$(ALIB)(nwxtbl.o) \
	$(ALIB)(print_selection.o) \
	$(ALIB)(readdata.o) \
	$(ALIB)(show_text.o) \
	$(ALIB)(skparams.o) \
	$(ALIB)(thermo.o) \
	$(ALIB)(winds.o) \
	$(ALIB)(xwvid1.o) \
	$(ALIB)(xwvid2.o) \
	$(ALIB)(xwvid3.o) \
	$(ALIB)(xwvid4.o) \
	$(ALIB)(xwvid5.o) \
	$(ALIB)(xwvid6.o) 

LOBJS   = $(FOBJS) $(COBJS)

# CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC) -I.
CFLAGS = $(COPT) $(GEMINC) -I.

all:	$(PROG)

$(PROG): $(ALIB) nsharp.c $(OS_LIB)/snlist.a $(OS_LIB)/sflist.a local_Nxmlib.a
	$(COMPILE.c) nsharp.c
	$(LINK.f) $(NOFORMAIN) -o $@ nsharp.o $(ALIB) $(GLIBS) $(X11LIBDIR) $(GUILIBS) $(SYSLIBS) -lm
	rm *.o

local_Nxmlib.a:
	cp $(OS_LIB)/Nxmlib.a $@
	$(AR) dv local_Nxmlib.a NxmPrt.o
	$(COMPILE.c) -I$(GEMPAK)/source/nxmlib -DNSHARP NxmPrt.c
	$(AR) $(ARFLAGS) local_Nxmlib.a NxmPrt.o

$(OS_LIB)/snlist.a :
	cd $(GEMPAK)/source/programs/sn/snlist ; \
	$(MAKE) $(MFLAGS) all

$(OS_LIB)/sflist.a :
	cd $(GEMPAK)/source/programs/sf/sflist ; \
	$(MAKE) $(MFLAGS) all

$(ALIB): $(COBJS) $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<

install: $(PROG)
	$(INSTALL) $? $(OS_BIN)/$?

clean:
	$(RM) *.o $(PROG) local_Nxmlib.a

snlist_clean:
	cd $(GEMPAK)/source/programs/sn/snlist ; \
	$(MAKE) $(MFLAGS) clean
sflist_clean:
	cd $(GEMPAK)/source/programs/sf/sflist ; \
	$(MAKE) $(MFLAGS) clean
