#
#	Makefile for nmap
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROG = ncolor

OBJS = \
	ncolor.o

DEVLIBS = \
	$(OS_LIB)/xwp.a \
	$(OS_LIB)/xw.a \
	$(PS) \
	$(GN)

LIBINC = \
	$(GPLT_NC) \
	$(NXMLIB) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(DEVLIBS) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC)

all : $(PROG)

$(PROG) : $(OBJS)
	$(LINK.f) $(NOFORMAIN) -o $@ $?\
		$(LIBINC) \
		$(GUILIBS) -lm

clean:
	$(RM) $(PROG) *.o 

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
