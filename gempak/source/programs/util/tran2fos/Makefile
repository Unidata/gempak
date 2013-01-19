include $(CONFIGDIR)/$(MAKEINC)

COBJS = \
	cstunp2.c

LOBJS =

ALIB =

PROG = tran2fos

all: $(PROG)

$(PROG):
	$(COMPILE.c) *.c
	$(LINK.c) -o $@ *.o \
		$(CGEMLIB)
	$(RM) *.o

install: all
	$(INSTALL) $(PROG) $(OS_BIN)

clean:
	$(RM) $(PROG) *.o
