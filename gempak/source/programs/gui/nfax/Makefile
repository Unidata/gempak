#
#	Makefile for nfax
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROG = nfax

OBJS = \
	vfiledisp.o \
	vfindload.o \
	vfonts.o \
	nfax.o \
	vmenucb.o \
	vmisccb.o \
	vrdbmap.o \
	vscrollcb.o \
	vshowmap.o \
	vtitle.o \
	vwheelcb.o

DEVLIBS = \
	$(XW) \
	$(GN)

LIBINC = \
	$(GPLT_NC) \
	$(NXMLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(DEVLIBS) \
	$(CGEMLIB)

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC)

all : $(PROG)

$(PROG) : $(OBJS)
	$(LINK.f) -o $@ $(NOFORMAIN) $(OBJS) $(LIBINC) $(GUILIBS) -lm

clean:
	$(RM) $(PROG) *.o

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
