#
#	Makefile for GEMPAK 6.0
#		Builds alternate gplt interface routines
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

OBJS = \
	gendp_alt.o \
	ginitp_alt.o

all : $(OBJS)

$(OBJS):
	$(COMPILE.f) $*.f
	$(CP) $@ $(OS_LIB)/$@
	$(RM) *.o

clean:
	$(RM) *.o 

