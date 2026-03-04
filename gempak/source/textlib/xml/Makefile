#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/textlib.a

COBJS = \
	$(ALIB)(xmlcount.o) \
	$(ALIB)(xmlgetsubdoc.o) \
	$(ALIB)(xmlreadtable.o) \
	$(ALIB)(xmltransform.o) \
	$(ALIB)(xmlutilities.o) \
	$(ALIB)(xmlvalue.o)

 
GDIR = xml

XML2_CFLAGS = $(shell xml2-config --cflags 2>/dev/null || pkg-config --cflags libxml-2.0 2>/dev/null)
XSLT_CFLAGS = $(shell xslt-config --cflags 2>/dev/null || pkg-config --cflags libxslt 2>/dev/null)

CFLAGS = $(COPT) -I$(OS_INC)/libxslt -I$(OS_INC)/libxml2 $(XML2_CFLAGS) $(XSLT_CFLAGS)

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

tlink : test$(GDIR).c
	$(COMPILE.c) test$(GDIR).c
	$(LINK.f) $(NOFORMAIN) test$(GDIR).o -o test$(GDIR) $(CGEMLIB) -lxml2 -lxslt
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

