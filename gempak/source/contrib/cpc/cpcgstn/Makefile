#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = \
	$(ALIB)(cpcgnewpoly.o) \
	$(ALIB)(cpcgrdbnd.o) \
	$(ALIB)(cpcgrdln.o) \
	$(ALIB)(cpcgsrch.o)

LOBJS = \
	$(ALIB)(cpcginp.o) \
	$(ALIB)(cpcgout.o)

ALIB = $(OS_LIB)/cpcgstn.a
 
PROG = cpcgstn
PROG.f = cpcgstn.f

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GN)

.PRECIOUS: $(ALIB)

FFLAGS = $(FOPT) $(NEED_U77)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) -lm -o $@

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o $(PROG) $(ALIB)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

