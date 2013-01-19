#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = 

ALIB = 
 
PROG = arinfo
PROG.c = arinfo.c
 

LIBINC = \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

.PRECIOUS: $(ALIB)

all : $(PROG)

$(PROG): $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c)
	$(LINK.f) $(NOFORMAIN) $@.o $(LIBINC) $(PNGLIB) $(ZLIB) -lm -o $@

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

