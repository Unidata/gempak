include $(CONFIGDIR)/$(MAKEINC)

version=v0.16.1

all: $(OS_LIB)/gettext_$(version)

_gettext: clean INSTALLDIR 
	@(cd $(version); echo "Making in `pwd`"; \
	./configure 'CC=' 'FC=' 'F77=' 'F90=' 'CPP=' 'CXX=' 'CXXCPP=' \
		'PKG_CONFIG=$(OS_BIN)/pkg-config' \
		'--prefix=$(OS_ROOT)' --disable-shared --disable-dependency-tracking \
		$(WITH-LIBICONV) ; \
	$(MAKE) all install; )

$(OS_LIB)/gettext_$(version):
	$(MAKE) _gettext
	-rm -f $(OS_LIB)/gettext_*
	touch $(OS_LIB)/gettext_$(version)

INSTALLDIR:
	@echo " Creating $(OS_ROOT)"
	@mkdir -p $(OS_ROOT)

install:

clean:
	-@cd $(version); $(MAKE) distclean
