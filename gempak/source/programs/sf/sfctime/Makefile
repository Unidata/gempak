include $(CONFIGDIR)/$(MAKEINC)

COBS = \
	sfctime.o

FOBS = \
	sfcopn.o

LIBINC = \
        $(GEMLIB) \
        $(CGEMLIB) \
        $(GEMLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB)


all: sfctime

sfctime: $(COBS) $(FOBS)
	$(LINK.f) $(NOFORMAIN) -o $@ $(COBS) $(FOBS) $(LIBINC) $(SYSLIBS)

$(COBS):
	$(COMPILE.c) $(@:.o=.c)

$(FOBS): 
	$(COMPILE.f) $(@:.o=.f)

install:
	cp sfctime $(OS_BIN)

clean:
	$(RM) $(FOBS) $(COBS) core sfctime

programs_gf:
