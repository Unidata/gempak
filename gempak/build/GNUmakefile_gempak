#
#  GNUmakefile_gempak
#
#  Use this in the GEMPAK directory ONLY.
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

SUBDIRS := source
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
