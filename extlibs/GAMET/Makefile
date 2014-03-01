#
#	Makefile for GEMPAK7
#		Follows each limb of source tree
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

all : 
	@( cd gametlib; echo "making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@ )

install: all

clean:
	@( cd gametlib; echo "making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@ )
