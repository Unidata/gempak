#
#	Makefile for GEMPAK7
#		Builds melbufr.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

AODTINC = $(OS_INC)/AODT/v64

all : includes

install: includes
	
clean:

includes: INCDIR
	@for inc in `ls *.h`; do \
		cmp -s $$inc $(AODTINC)/$$inc || \
		( $(CP) $$inc $(AODTINC)/$$inc  && echo updated $(AODTINC)/$$inc ) ; \
	done

INCDIR:
	@if [ ! -d $(AODTINC) ]; then mkdir -p $(AODTINC); fi

