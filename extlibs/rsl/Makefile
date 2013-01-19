#
#	Makefile for GEMPAK 6.0
#		Follows each limb of source tree
#		Unidata 11/99 Chiz
#

SUBDIRS = \
	src

all :
	@for dir in $(SUBDIRS); do \
	   cd $$dir; pwd; $(MAKE) $(MFLAGS) $@; \
	done

install:

clean:
	@for dir in $(SUBDIRS); do \
	   cd $$dir; pwd; $(MAKE) $(MFLAGS) $@; \
	done

