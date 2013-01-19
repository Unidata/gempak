# Top level makefile to build windco

SHELL=		/bin/sh

DIRAPI=odtappl
DIRSUBS=odtlib odtapi odtext odtmcidas odtmcidas/navcal/navcal odtmcidas/navcal/libf2c

subs:
	@$(MAKE) SUBDIRS="$(DIRSUBS)" TARG="all" subd

appl:
	@$(MAKE) SUBDIRS="$(DIRAPI)" TARG="naodt" subd


# target for recursive directory make
subd:
	@for dir in $(SUBDIRS); do \
	  (cd $$dir; echo Making \`$(TARG)\' in `pwd`; \
	  $(MAKE) $(TARG)); \
	done

tar:
	rm -f naodt.tar.gz
	tar -cvf naodt.tar odtlib/* odtapi/* odtext/* \
	         odtmcidas/* odtappl/* inc/* \
	         TOPOLRES aodtenv compileall Makefile Updates ProgMan ProgMan-mcidas
	/usr/bin/gzip naodt.tar

clean:
	rm -r -f odtlib/*.o odtlib/*.a
	rm -r -f odtapi/*.o odtapi/*.a
	rm -r -f odtappl/*.o odtappl/*.a
	rm -r -f odtext/*.o odtext/*.a
	rm -r -f odtmcidas/*.o odtmcidas/*.a
	rm -r -f odtmcidas/navcal/navcal/*.o odtmcidas/navcal/navcal/*.a
	rm -r -f odtmcidas/navcal/libf2c/*.o odtmcidas/navcal/libf2c/*.a
	rm -r -f naodt
	rm -r -f lib/*.a
