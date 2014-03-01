#
#	Makefile for GEMPAK7
#		Builds libaodt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/libgpc.a

COBJS = \
	$(ALIB)(gpc.o)
 
.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<

install:

clean:
	$(RM) *.o 

programs_nc:

programs_gf:

