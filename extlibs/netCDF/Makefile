include $(CONFIGDIR)/$(MAKEINC)
include makefile.common

NETCDF_CFLAGS = -O2 -w
NETCDF_CPPFLAGS = -w

NETCDF            = $(OS_LIB)/libnetcdf.a


all: $(NETCDF)

install:

$(NETCDF):
	@echo look netcdf $(NETCDF)
	$(MAKE) lnetcdf $(NCOPT) $(NCENV)

lnetcdf: INSTALLDIR
	-@cd netcdf; \
	pwd; \
	touch macros.make; \
	$(MAKE) distclean ; \
	./configure --disable-dap '--prefix=$(OS_ROOT)' 'CFLAGS=$(NETCDF_CFLAGS)' 'CPPFLAGS=$(NETCDF_CPPFLAGS)'; \
	$(MAKE) -s all install

INSTALLDIR:
	@echo " Creating $(OS_ROOT)"
	@mkdir -p $(OS_ROOT)

clean:
	-@cd netcdf; $(MAKE) $(MFLAGS) distclean
