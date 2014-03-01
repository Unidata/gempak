#
#	Makefile for GEMPAK7
#		Follows each limb of source tree
#		Unidata 11/99 Chiz
#

SUBDIRS = \
	include \
	src

all :
	@for dir in $(SUBDIRS); do \
	   cd $$dir; pwd; $(MAKE) $(MFLAGS) $@; cd .. ; \
	done

install:
	@for dir in $(SUBDIRS); do \
	   cd $$dir; pwd; $(MAKE) $(MFLAGS) $@; cd .. ; \
	done

clean:
	@for dir in $(SUBDIRS); do \
	   cd $$dir; pwd; $(MAKE) $(MFLAGS) $@; cd .. ; \
	done

