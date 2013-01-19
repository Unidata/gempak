include $(CONFIGDIR)/$(MAKEINC)

COBJS =

LOBJS =

ALIB =

PROG = tpc2shp

all: $(PROG)

#
# may nead -lpthread
$(PROG): $(PROG).c
	$(COMPILE.c) $@.c
	$(LINK.f) $(NOFORMAIN) -o $@ $@.o \
		$(OS_LIB)/textlib.a $(CGEMLIB) $(GEMLIB) $(CGEMLIB)  \
		$(LIBGPC) $(GPLT_NC) \
		$(GPLT) $(DEVICE) $(GN) $(GEMLIB) $(CGEMLIB) $(GEMLIB) $(OS_LIB)/libshp.a \
		$(ZLIB) -lm
	$(RM) *.o

install: all
	$(INSTALL) $(PROG) $(OS_BIN)

clean:
	$(RM) $(PROG) *.o
