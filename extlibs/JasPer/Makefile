#
#	Makefile for GEMPAK 6.0
#		Follows each limb of source tree
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

all : $(LIBJASPER)

install: all

clean: _jasper_clean

$(LIBJASPER): jasper/configure
	make _jasper

_jasper :
	@-cd jasper; \
	pwd; \
	$(MAKE) distclean; \
	./configure '--prefix=$(OS_ROOT)' $(JASPEROPT) ; \
	$(MAKE) all install

_jasper_clean:
	@-cd jasper; $(MAKE) distclean

