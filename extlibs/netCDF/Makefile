include $(CONFIGDIR)/$(MAKEINC)
include makefile.common

NETCDF_CFLAGS = -O2 -w
NETCDF_CPPFLAGS = -I$(OS_INC) -w

NETCDF            = $(OS_LIB)/libnetcdf.a


all: $(NETCDF)

install:

$(NETCDF):
	@echo look netcdf $(NETCDF)
	$(MAKE) lnetcdf $(NCOPT) $(NCENV) || exit $$?

lnetcdf: INSTALLDIR
	@cd netcdf; \
	pwd; \
	touch macros.make; \
	if [ -f Makefile ]; then $(MAKE) distclean ; fi ; \
	./configure --disable-dap '--prefix=$(OS_ROOT)' 'CFLAGS=$(NETCDF_CFLAGS)' 'CPPFLAGS=$(NETCDF_CPPFLAGS)'; \
	$(MAKE) -s all install
	@test -f $(OS_INC)/netcdf.h || (echo "ERROR: expected $(OS_INC)/netcdf.h after netCDF install" >&2; exit 1)

INSTALLDIR:
	@echo " Creating $(OS_ROOT)"
	@mkdir -p $(OS_ROOT)

clean:
	-@cd netcdf; $(MAKE) $(MFLAGS) distclean
