#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = 

COBJS = 

FOBJS = $(ALIB)(ggcwcp.o)

PROG = woupdt
PROG.f = woupdt.f

LIBINC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(BRIDGE) \
	$(GPLT) \
	$(DEVICE) \
	$(GN) \
	$(GEMLIB) \
        $(XMLLIB) \
	$(ZLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

all : $(PROG)

$(PROG): $(LIBINC) $(PROG.f)
	$(LINK.f) $(NEED_U77) $(PROG.f) $(LIBINC) $(SYSLIBS) -lm -o $@

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<

clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

