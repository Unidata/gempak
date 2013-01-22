#
#	Makefile for GEMPAK 6.0
#		Follows each limb of source tree
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

all : $(LIBBZ2)

install: all

clean: _bz2_clean

$(LIBBZ2):
	( cd bzip2; echo "making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) libbz2.a )

_bz2: $(LIBBZ2)

_bz2_clean:
	( cd bzip2; echo "making $@ in `pwd`" ; \
		if ( test -f Makefile ) ; then make clean ; fi )


