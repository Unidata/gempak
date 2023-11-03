#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = \
	$(ALIB)(namcdt.o) \
	$(ALIB)(namcfc.o) \
	$(ALIB)(naminp.o) \
	$(ALIB)(naminv.o) \
	$(ALIB)(namjen.o) \
	$(ALIB)(namlsd.o) \
	$(ALIB)(namopn.o) \
	$(ALIB)(namprp.o) \
	$(ALIB)(namrbf.o) \
	$(ALIB)(namrdd.o) \
	$(ALIB)(namsst.o)

ALIB = $(OS_LIB)/namsnd.a
 
PROG = namsnd
PROG.f = namsnd.f
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
        $(XMLLIB) \
        $(ZLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(SYSLIB) \
	$(LIBNCEPBUFR) \
	$(LIBREADLINE)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

programs_gf:

programs_nc: 

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(CSYSLIB) -o $@

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

