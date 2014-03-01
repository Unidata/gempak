#
#	Makefile for GEMPAK7
#		Builds melbufr.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

all : includes

install: includes
	
clean:

includes:
	@for inc in `ls *.h`; do \
		cmp -s $$inc $(OS_INC)/$$inc || \
		( $(CP) $$inc $(OS_INC)/$$inc  && echo updated $(OS_INC)/$$inc ) ; \
	done

