#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = \
	afbufr_common.o \
	afcreatebufr.o \
	afcreatexml.o

LOBJS = 

ALIB = 
 
PROG = airmet_vgfbufr

PROG.c = airmet_vgfbufr.c

LIBINC = \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(LIBGPC) \
	$(GPLT_NC) \
	$(GPLT) \
	$(DEVICE) \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(OS_LIB)/libmel_bufr.a \
	$(XMLLIB) \
	$(ZLIB)

.PRECIOUS: $(ALIB)

CFLAGS = $(COPT) $(BUFRFLAGS) \
        -DDEBUG_PRINT=1 \
        -DFNOC_KLUDGE=0 \
        -DIGNORE_NESTING=1 \
        -DMEMORY_DEBUG=0 \
        -DTRACE_PRINT=1 \
        -I$(OS_INC)/libxml2


all : $(ALIB) $(PROG)

$(PROG): $(COBJS) $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c)
	$(LINK.f) $(NOFORMAIN) $@.o $(COBJS) $(LIBINC) $(XMLDEP) -lm -lrt -o $@

clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

