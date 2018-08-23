include $(CONFIGDIR)/$(MAKEINC)


all: $(OS_LIB)/libhdf5.a

install:

$(OS_LIB)/libhdf5.a:
	@echo look hdf5 $(HDFOPT)
	$(MAKE) _lhdf5 $(HDFOPT)

_lhdf5: INSTALLDIR
	-@cd libhdf; \
	pwd; \
	$(MAKE) distclean ; \
	./configure '--prefix=$(OS_ROOT)' '--with-zlib=$(OS_ROOT)' --disable-debug --disable-trace ; \
	$(MAKE) all install

INSTALLDIR:
	@echo " Creating $(OS_ROOT)"
	@mkdir -p $(OS_ROOT)

clean:
	-@cd libhdf; $(MAKE) $(MFLAGS) distclean
