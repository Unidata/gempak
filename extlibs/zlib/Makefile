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
	( set -e; cd zlib; echo "making $@ in `pwd`"; \
		if [ -f Makefile ]; then $(MAKE) -s distclean; fi; \
		( autoreconf --force --install || touch * ); \
		./configure --static -s --prefix=$(OS_ROOT); \
		$(MAKE) -s all install; \
		$(MAKE) -s distclean )

_zlib: $(ZLIB)

_zlib_clean:
	( cd zlib; echo "making $@ in `pwd`" ; \
		make -s distclean )


