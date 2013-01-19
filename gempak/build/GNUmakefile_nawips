#
#  GNUmakefile_nawips
#
#  Use this in the NAWIPS directory ONLY.
#
SHELL = /bin/sh

#
#  Define PREFIX if not already defined.
#
PREFIX ?= $(OS_ROOT)

#
#  Define LIBDIR if not already defined.
#
LIBDIR ?= $(PREFIX)/lib

SUBDIRS := extlibs/GAMET extlibs/GPC gempak 
TARGETS := all check distclean link

include $(GEMINC)/subdirs.mk

check::
	@echo ""
	@echo "SUBPATH = $(SUBPATH)"
	@echo "SUBDIRS = $(SUBDIRS)"
	@echo "TARGETS = $(TARGETS)"
	@echo ""

clean::
	-$(RM) -r $(LIBDIR)/obj

.PHONY: check clean
