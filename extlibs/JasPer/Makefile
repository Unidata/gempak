#
#	Makefile for GEMPAK7
#		Follows each limb of source tree
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

all : $(LIBJASPER)

clean: _jasper_clean

$(LIBJASPER): jasper/configure
	make _jasper

_jasper :
	@-cd jasper; \
	pwd; \
	$(MAKE) distclean; \
	export echo=echo
	./configure '--prefix=$(OS_ROOT)' $(JASPEROPT) ; \
	$(MAKE) all install

_jasper_clean:
	@-cd jasper; $(MAKE) distclean

