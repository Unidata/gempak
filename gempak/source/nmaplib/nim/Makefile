#
#	Makefile for nmaplib
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/nmaplib.a

FOBJS = 

COBJS = \
	$(ALIB)(nimflnm.o) \
	$(ALIB)(nimgfil.o) \
	$(ALIB)(nimgtim.o) \
	$(ALIB)(niminit.o) \
	$(ALIB)(nimplot.o) \
	$(ALIB)(nimqatt.o) \
	$(ALIB)(nimrest.o) \
	$(ALIB)(nimsatt.o) \
	$(ALIB)(nimsave.o)

.PRECIOUS: $(ALIB)

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC) 

all : $(ALIB)

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o


.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


install: 

tlink : testnim.c
	$(COMPILE.c) $?
	$(LINK.f) testnim.o -o testnim $(ALIB) \
	$(GEMLIB) $(CGEMLIB) $(APPL) $(SYSLIB) $(GEMLIB) $(NETCDF) $(ZLIB) $(RSLLIB) -lm
	$(INSTALL) testnim $(OS_BIN)
	$(RM) *.o testnim

clean:
	$(RM) *.o 
