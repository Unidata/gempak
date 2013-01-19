#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	af \
	am \
	br \
	cs \
	dc \
	fa \
	ff \
	hc \
	is \
	ls \
	ma \
	mn \
	ms \
	mt \
	nc \
	ra \
	rd \
	rs \
	ru \
	sc \
	sh \
	si \
	sv \
	tf \
	wc \
	wn \
	wo \
	wp \
	ws \
	ww

all :
	@for dir in $(SUBDIRS) ; do \
	   (cd $$dir; echo "making $@ in `pwd` " ; \
		$(MAKE) $(MFLAGS) $@) ; $(ARSLEEP) \
	done

install :

clean:
	@for dir in $(SUBDIRS) ; do \
	   (cd $$dir; echo "making $@ in `pwd` " ; \
		$(MAKE) $(MFLAGS) $@) ; \
	done
