include $(CONFIGDIR)/$(MAKEINC)

PROGS = nmap_kiosk

all: $(PROGS)

nmap_kiosk: nmap_kiosk.c
	$(COMPILE.c) $?
	$(LINK.c) -o $@ *.o $(X11LIBDIR) -lXtst -lX11 -lXext
	$(RM) *.o

install: all
	$(INSTALL) $(PROGS) $(OS_BIN)
	$(AUXINSTALL) run_kiosk.csh $(NAWIPS)/bin

clean:
	$(RM) $(PROGS) *.o
