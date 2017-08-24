include $(CONFIGDIR)/$(MAKEINC)


all: $(OS_LIB)/libxslt.a

$(OS_LIB)/libxslt.a:
	$(MAKE) $(MFLAGS) _xslt

_xslt: clean INSTALLDIR 
	@(cd libxslt; echo "Making in `pwd`"; \
	./configure '--prefix=$(OS_ROOT)' --disable-static --without-debug --without-debugger '--with-libxml-prefix=$(OS_ROOT)' 'CC=$(CC)' ; \
	$(MAKE) install )

INSTALLDIR:
	@echo " Creating $(OS_ROOT)"
	@mkdir -p $(OS_ROOT)

install:

clean:
	-@cd libxslt; $(MAKE) distclean
