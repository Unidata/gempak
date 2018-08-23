#
#	Makefile for GEMPAK
#		Follows each limb of source tree
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

all : $(ZLIB)

install: all

clean: _zlib_clean

$(ZLIB):
	( cd zlib; echo "making $@ in `pwd`" ; \
		make -s distclean ; \
		CC=$(CC) && export CC && ./configure --static -s --prefix=$(OS_ROOT); \
		make -s all install ; \
		make -s distclean )

_zlib: $(ZLIB)

_zlib_clean:
	( cd zlib; echo "making $@ in `pwd`" ; \
		make -s distclean )


