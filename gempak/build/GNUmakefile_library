#
#  N-AWIPS GNUmakefile_library
#
#	Used when the entire library is
#	contained in a single directory;
#	e.g., $GEMPAK/source/nxmlib.
#
#  IMPORTANT:   This description file assumes that the
#		library or program name is the same as
#		the root library or program directory
#		name.  (see DIRNAM below)
#
#		This description file assumes that the
#		program names produced are determined
#		from the names of the '.mk' files found
#		in the program directory.  (see 
#		$GEMINC/link.mk file)
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

#
#  OBJDIR is where we want to store the object (*.o)
#	and depend files (*.u) ... it must be what
#	is desired the first time through!
#
#	Remember, the first time through this file is
#	being executed from its original, SRCDIR,
#	location; while the second time through
#	it is being executed from OBJDIR!
#
#  OBJDIR is defined this way because the library
#	does NOT have any subdirectories.
#
OBJDIR := $(LIBDIR)/obj/$(notdir $(CURDIR))_obj

#
#  Magic to process the makefile from OBJDIR.
#
#  IMPORTANT:  For the magic to work the variable
#		OBJDIR must exist and have the
#		suffix "_obj".
#
ifeq (, $(filter %_obj,$(notdir $(CURDIR))))

include $(GEMINC)/target.mk

else

#
#  Start of "normal" description file that
#  is being executed from OBJDIR.
#
#  VPATH allows us to locate the source
#  files from OBJDIR.
#
#  SRCDIR is defined in target.mk.
#
VPATH  := $(SRCDIR)
LOCDIR := $(notdir $(SRCDIR))
DIRNAM := $(notdir $(shell cd $(SRCDIR); pwd))

#
#  curlib.mk is the 'all' target to compile
#	all available source code and
#	create an archive library in $LIBDIR.
#
include $(GEMINC)/curlib.mk

#
#  link.mk is the 'link' target to link
#	all available programs in $BINDIR.
#
include $(GEMINC)/link.mk

#
#  check.mk is the 'check' target
#	to display key variables.
#
include $(GEMINC)/check.mk

#
#  clean target:  remove object and dependency files...
#
clean::
	-$(RM) $(OBJCTS) $(patsubst %.o,%.u, $(OBJCTS))

distclean:: clean
	-$(RM) $(CURLIB)

#
#  depend.mk contains the logic to create 
#	dependencies for all source files.
#
include $(GEMINC)/depend.mk

#
#  PHONY forces these non-file targets to execute.
#
.PHONY: clean distclean

endif
