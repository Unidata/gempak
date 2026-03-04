include $(CONFIGDIR)/$(MAKEINC)

HDF5_CFLAGS = -O2 -w
HDF5_CPPFLAGS = -w


all: $(OS_LIB)/libhdf5.a

install:

$(OS_LIB)/libhdf5.a:
	@echo look hdf5 $(HDFOPT)
	$(MAKE) _lhdf5 $(HDFOPT)

_lhdf5: INSTALLDIR
	@cd libhdf; \
	pwd; \
	if [ -f Makefile ]; then $(MAKE) distclean ; fi ; \
	./configure '--prefix=$(OS_ROOT)' '--with-zlib=$(OS_ROOT)' --disable-debug --disable-trace 'CFLAGS=$(HDF5_CFLAGS)' 'CPPFLAGS=$(HDF5_CPPFLAGS)' ; \
	$(MAKE) -s all install

INSTALLDIR:
	@echo " Creating $(OS_ROOT)"
	@mkdir -p $(OS_ROOT)

clean:
	-@cd libhdf; $(MAKE) $(MFLAGS) distclean
