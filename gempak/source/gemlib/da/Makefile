#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/libxml2.a

CFLAGS += -I$(OS_INC)/libxml2

COBJS = \
	$(ALIB)(dareadxml.o) \
	$(ALIB)(darunpy.o)

GDIR = da

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS) 
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

clean:
	$(RM) *.o 
