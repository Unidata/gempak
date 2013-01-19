#
#  N-AWIPS GNUmakefile_obj
#
#  Currently this is used only for
#	$NAWIPS/gempak/source/gplt/access/alt.
#
#  IMPORTANT:  This description file assumes that
#	the library name is the same as the root
#	library directory name.
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
#  OBJDIR is defined this way because this is a sub
#	sub directory of the library being built.
#
OBJDIR := $(LIBDIR)/obj/$(notdir $(shell cd ../..; pwd))_obj

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

#
#  Define GEMDIR and INCDIR if not already defined.
#
GEMDIR ?= $(PREFIX)/../../gempak/include
INCDIR ?= $(PREFIX)/include

#
#  Add GEMDIR and INCDIR to CFLAGS and FFLAGS so
#	developer's include files are found before
#	the $GEMINC and $OS_INC include files.
#
CFLAGS := -I$(GEMDIR) -I$(INCDIR) $(CFLAGS)
FFLAGS := -I$(GEMDIR) -I$(INCDIR) $(FFLAGS)

#
#  If unique flags are needed add
#  them in a cflags.mk file.
#
-include $(SRCDIR)/cflags.mk

#
#  Create a list of all '.c' and '.f' files.
#
CSORCS := $(notdir $(wildcard $(SRCDIR)/*.c))
FSORCS := $(notdir $(wildcard $(SRCDIR)/*.f))

#
#  Convert list of sources to a list of objects.
#
OBJCTS := $(patsubst %.c,%.o, $(CSORCS)) \
	  $(patsubst %.f,%.o, $(FSORCS))

#
#  Eliminate 'main' and 'PROGRAM' files from
#	list of objects to create library.
#
LIBOBJ := $(filter-out test%, $(OBJCTS))
LIBOBJ := $(filter-out $(LOCDIR).o, $(LIBOBJ))

#
#  If source (object) file(s) exist continue processing.
#
ifeq "$(words $(LIBOBJ))" "0"

all:: ;

else

#
#  C compile rule to ensure all compilers work.
#
%.o: %.c
	$(CC) $(CFLAGS) -c $<

#
#  FORTRAN compile rule to ensure MCHPRM.PRM exists.
#
%.o: %.f
	@[[ -d $(OS_INC) ]] || mkdir -p $(OS_INC); \
	[[ -e $(OS_INC)/MCHPRM.PRM ]] || \
	ln -s $(GEMINC)/MCHPRM.$(OS) $(OS_INC)/MCHPRM.PRM
	$(FC) $(FFLAGS) -c $<

#
#  all target:  process source into objects...
#
all:: $(addprefix $(OS_LIB)/, $(LIBOBJ))

$(addprefix $(OS_LIB)/, $(LIBOBJ)): $(LIBOBJ)
	cp $? $(LIBDIR)

endif

#
#  Nothing to link in a sub sub directory.
#
link:: ;

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
	-$(RM) $(addprefix $(OS_LIB)/, $(LIBOBJ))

#
#  depend.mk contains the logic to create
#	dependencies for all source files.
#
include $(GEMINC)/depend.mk

#
#  PHONY forces these non-file targets to execute.
#
.PHONY: all clean distclean link

endif
