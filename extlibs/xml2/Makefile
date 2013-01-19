include $(CONFIGDIR)/$(MAKEINC)


all: $(OS_LIB)/libxml2.a

$(OS_LIB)/libxml2.a:
	$(MAKE) $(MFLAGS) _xml2

_xml2: clean INSTALLDIR 
	@(cd libxml2; echo "Making in `pwd`"; \
	./configure '--prefix=$(OS_ROOT)' --disable-shared --without-debug '--with-zlib=$(OS_ROOT)' $(XML2OPT); \
	$(MAKE) install )

INSTALLDIR:
	@echo " Creating $(OS_ROOT)"
	@mkdir -p $(OS_ROOT)

install:

clean:
	-@cd libxml2; $(MAKE) distclean
