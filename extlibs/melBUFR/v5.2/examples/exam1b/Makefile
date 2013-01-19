######################################################################
# To customize edit lines with: PROGRAM, DEST, SRCS, HDRS, LIBS,
#				OTHER_LIBS, INCLUDES
#
# Targets are:
#	program - same as above
#	install - makes and installs program in destination
#	clean 	- removes .o files but not executable program
#	clobber - removes .o files and executable program
#
# Debugging can be accomplished with the command: 
#	make "DFLAGS=-g"
######################################################################
#
# Define C compiler used
#

############## SGI ####################
# ANSI C compiler
CC = cc
#COMPILER_FLAGS= -O -wlint
COMPILER_FLAGS= -O -ansi
########################################

##############################################################################
# Debugging flags
#
# If value is set =1:
#
# DEBUG_PRINT    - Allow debug printing
# FNOC_KLUDGE    - Allow FNOC-specific kludges while decoding messages.
# IGNORE_NESTING - Ignore nesting of 2-01-YYY and 2-02-YYY descriptors
#                  while decoding messages.
# MEMORY_DEBUG   - Used for finding memory leaks.  CAUTION: makes for 
#                  extremely SLOW execution speed!
# TRACE_PRINT    - Allow/disallow trace printing

DEBUG_FLAGS = \
	-DDEBUG_PRINT=0 \
	-DFNOC_KLUDGE=1 \
	-DIGNORE_NESTING=1 \
	-DMEMORY_DEBUG=0 \
	-DTRACE_PRINT=0

##############################################################################

# program name
PROGRAM	= ex1

# source files
SRCS          = ex1.c

# object files =

OBJS	      = $(SRCS:.c=.o)

# local headers that your code depends on
HDRS	      = 

# libraries checked in dependency rules, need full path

LIBS	      = $(MEL_BUFR)/lib/libmel_bufr.a 

# other libraries such as -lX11 -lm  etc. and -L directives
OTHER_LIBS    = 

# include search paths
INCLUDES      = -I$(MEL_BUFR)/include

# compile and link flags
DFLAGS	      = 
CFLAGS        = $(COMPILER_FLAGS) $(DFLAGS) $(INCLUDES)
SHELL	      = /bin/sh
RM 	      = /bin/rm -f
INSTALL       = mv

.c.o:	$(HDRS)
	@echo Making $<
	$(CC) -c $(CFLAGS) $<

all: $(PROGRAM)

$(PROGRAM): $(LIBS) $(OBJS)
	@echo
	@echo Linking $(PROGRAM)
	$(CC) $(CFLAGS)  $(OBJS) -o $@ $(LIBS) $(OTHER_LIBS)
	
# create program and install in destination directory
install:	$(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)/$(PROGRAM)
		@if [ $(DEST) != . ]; then \
		($(RM) $(DEST)/$(PROGRAM); $(INSTALL) $(PROGRAM) $(DEST)); fi
clean:
	@$(RM) *.o core

clobber:
	@$(RM) $(PROGRAM) *.o core nn_*[!c]
