#
#       Makefile for GEMPAK 6.0
#               Builds program makefiles
#               Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

LIBINC =

COBJS = 

LOBJS = 

PROG = freeColors

CFLAGS = $(COPT) $(XWINCDIR)

all : $(PROG)

$(PROG):
	$(COMPILE.c) $@.c
	$(LINK.c) $@.o $(X11LIBDIR) -lX11  -o $@

clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
