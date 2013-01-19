include $(CONFIGDIR)/$(MAKEINC)

FC=""
F77=""
F90=""
CPP=""
CXX=""
CXXCPP=""

version=v0.21

all: $(OS_BIN)/pkg-config_$(version)

_pkg-config: clean INSTALLDIR 
	@(cd $(version); echo "Making in `pwd`"; \
	./configure '--prefix=$(OS_ROOT)' --disable-shared --disable-dependency-tracking 'CC=$(CC)' \
		FC="" F77="" F90="" CPP="" CXX="" CXXCPP="" $(PKG-CONFIG-OPT); \
	$(MAKE) all install )

$(OS_BIN)/pkg-config_$(version):
	$(MAKE) _pkg-config
	-rm -f $OS_BIN/pkg-config_*
	touch $(OS_BIN)/pkg-config_$(version)

INSTALLDIR:
	@echo " Creating $(OS_ROOT)"
	@mkdir -p $(OS_ROOT)

install:

clean:
	-@cd $(version); $(MAKE) distclean
