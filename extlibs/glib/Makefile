include $(CONFIGDIR)/$(MAKEINC)

all: _glib

_glib: clean INSTALLDIR 
	@(cd src; echo "Making in `pwd`"; \
	./configure 'CC=' 'LDFLAGS=-L$(OS_LIB)' 'CPPFLAGS=-DNDEBUG -I$(OS_INC)' \
		'FC=' 'F77=' 'F90=' 'CPP=' 'CXX=' 'CXXCPP=' 'PKG_CONFIG=$(OS_BIN)/pkg-config' \
		'--prefix=$(OS_ROOT)' --disable-shared --disable-dependency-tracking  \
		--disable-debug $(WITH-LIBICONV); \
	$(MAKE) all install )



INSTALLDIR:
	@echo " Creating $(OS_ROOT)"
	@mkdir -p $(OS_ROOT)

install:

clean:
	-@cd src; $(MAKE) distclean
