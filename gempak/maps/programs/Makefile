#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = $(ALIB)(map_gsfssf.o)

ALIB = $(OS_LIB)/gsfssf.a
 
PROG = gsfssf
PROG.c = $(PROG).c

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

programs_nc:
	@echo "Nothing to make for programs_nc in gui"

programs_gf:
	@echo "Nothing to make for programs_gf in gui"


$(PROG): $(ALIB) $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c)
	$(LINK.f) $@.o $(ALIB) $(LIBINC) -o $@

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(CFLAGS) -c $<

clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

