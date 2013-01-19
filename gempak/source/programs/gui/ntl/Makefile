#
#	Makefile for ntl
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROG = ntl

COBJS = \
	ntl.o \
	appbtn.o \
	apptab.o \
	prog.o \
	colr.o


LIBINC = \
	$(NXMLIB) \
	$(OS_LIB)/xw.a \
	$(GN) \
	$(CGEMLIB) \
	$(GEMLIB)

LOCALINCLUDE = -I.

CFLAGS = $(COPT) $(MOTIFINC) $(XWINCDIR)

all : $(PROG)

$(PROG) : $(COBJS)
	$(LINK.c) -o $@ \
		$(COBJS) \
		$(LIBINC) \
		$(GUILIBS) \
		$(SYSLIBS)

clean:
	$(RM) $(PROG) $(COBJS)



install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
