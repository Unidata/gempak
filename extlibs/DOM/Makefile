include $(CONFIGDIR)/$(MAKEINC)

version=v2-0.8.1

all: $(OS_LIB)/DOM_$(version)

_DOM: clean INSTALLDIR 
	@(cd $(version); echo "Making in `pwd`"; \
	./configure '--prefix=$(OS_ROOT)' '--disable-shared' '--disable-dependency-tracking' \
		'PKG_CONFIG=$(OS_BIN)/pkg-config' '--with-glib-prefix=$(OS_ROOT)' \
		'--with-libxml-prefix=$(OS_ROOT)' ; \
	$(MAKE) all install )

$(OS_LIB)/DOM_$(version):
	$(MAKE) _DOM
	-rm -f $(OS_LIB)/DOM_*
	touch $(OS_LIB)/DOM_$(version)

INSTALLDIR:
	@echo " Creating $(OS_ROOT)"
	@mkdir -p $(OS_ROOT)

install:

clean:
	-@cd $(version); $(MAKE) distclean
