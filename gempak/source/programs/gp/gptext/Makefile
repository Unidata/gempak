#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = \
	$(ALIB)(gptinp.o) \
	$(ALIB)(gptopt.o)

ALIB = $(OS_LIB)/gptext.a
 
PROG = gptext
PROG.f = gptext.f
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(GEMLIB)

LIBINC_NC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(NC) \
	$(GN) \
	$(CGEMLIB) \
	$(GPLT) \
	$(GEMLIB)

LIBINC_VG = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(VG) \
	$(GN) \
	$(CGEMLIB) \
	$(GPLT) \
	$(GEMLIB)

PROG_NC = $(PROG)_nc
PROG_VG = $(PROG)_vg

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) -o $@

$(PROG_NC) : $(ALIB) $(LIBINC_NC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_NC) -o $(OS_BIN)/$@

$(PROG_VG) : $(ALIB) $(LIBINC_VG) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_VG) -o $(OS_BIN)/$@

programs_gf:
	@echo "Nothing to make for _gf"

programs_nc: $(PROG_NC)

programs_vg: $(PROG_VG)

$(ALIB): $(LOBJS) $(COBJS)
	-$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

