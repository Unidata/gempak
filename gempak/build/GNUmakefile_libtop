#
#  N-AWIPS GNUmakefile_libtop
#
#  Use this in directories that are a
#	root (top directory) for a library.
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

LOCDIR := $(notdir $(CURDIR))
OBJDIR := $(LIBDIR)/obj/$(LOCDIR)_obj
CURLIB := $(LIBDIR)/lib$(LOCDIR).a

#
#  Process subdirectories...
#
TARGETS := all check link
include $(GEMINC)/subdirs.mk

#
#  Other targets...
#
check::
	@echo ""
	@echo "SUBPATH = $(SUBPATH)"
	@echo "SUBDIRS = $(SUBDIRS)"
	@echo "TARGETS = $(TARGETS)"
	@echo ""

clean::
	-$(RM) -r $(OBJDIR)

distclean:: clean
	-$(RM)    $(CURLIB)

#
#  PHONY forces these non-file targets to execute.
#
.PHONY: check clean distclean
