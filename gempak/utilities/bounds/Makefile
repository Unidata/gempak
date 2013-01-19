include $(CONFIGDIR)/$(MAKEINC)

CFLAGS = $(COPT) -I$(GEMPAK)/source/cgemlib/clo

all: createbinfo vgftobnd chgCWAcentroid

install: all
	$(INSTALL) createbinfo $(OS_BIN)
	$(INSTALL) vgftobnd $(OS_BIN)
	$(INSTALL) chgCWAcentroid $(OS_BIN)

createbinfo:
	$(COMPILE.c) $@.c
	$(LINK.c) -o $@ $@.o $(CGEMLIB)
	$(RM) $@.o

vgftobnd:
	$(COMPILE.c) $@.c
	$(LINK.f) $(NOFORMAIN) -o $@ $@.o $(CGEMLIB) $(GEMLIB) $(APPL) $(CGEMLIB) $(GEMLIB) $(SYSLIB) -lm
	$(RM) $@.o

chgCWAcentroid:
	$(COMPILE.c) $@.c
	$(LINK.c) -o $@ $@.o $(GEMLIB) $(CGEMLIB) $(GPLT) $(GEMLIB)
	$(RM) $@.o

clean:
	$(RM) createbinfo vgftobnd chgCWAcentroid *.o
