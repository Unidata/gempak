include $(CONFIGDIR)/$(MAKEINC)

LIBS = -L$(OS_LIB) -lncepBUFR

PROGS = cwordsh

all: $(PROGS)

install: $(PROGS)
	$(INSTALL) $? $(OS_BIN)

cwordsh:
	$(LINK.f) -o $@ $@.f $(LIBS)

clean:
	$(RM) *.o $(PROGS) 20070208.* \
	bufr.725650.2007020812.unblock \
	bufr.725650.2007020812.reblock

test: $(PROGS)
	./reblock.csh
