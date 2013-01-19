#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/libshp.a

COBJS = \
	$(ALIB)(dbfopen.o) \
	$(ALIB)(shpopen.o)
 

.PRECIOUS: $(ALIB)

all : $(ALIB) includes

$(ALIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

includes:
	@for inc in `ls shapefil.h`; do \
		cmp -s $$inc $(OS_INC)/$$inc || \
		( $(CP) $$inc $(OS_INC)/$$inc  && echo updated $(OS_INC)/$$inc ) ; \
	done

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

