#
#	Makefile for GEMPAK 6.0
#		Follows each limb of source tree
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

all : $(PNGLIB)

install: all

clean: _png_clean

$(PNGLIB):
	$(MAKE) $(MFLAGS) _png

INSTALLDIR:
	@echo " Creating $(OS_ROOT)"
	@mkdir -p $(OS_ROOT)

_png: clean INSTALLDIR
	@( cd libpng; echo "making $@ in `pwd`" ; \
	./configure '--prefix=$(OS_ROOT)' --disable-shared $(CONFIGURE_OPTS); \
	$(MAKE) all install )

_png_clean:
	-@( cd libpng; echo "making $@ in `pwd`" ; $(MAKE) distclean )


