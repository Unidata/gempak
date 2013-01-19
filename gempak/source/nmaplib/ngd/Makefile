#
#	Makefile for nmaplib
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/nmaplib.a

FOBJS = \
	$(ALIB)(ngddspl.o) \
	$(ALIB)(ngdnlst.o) \
	$(ALIB)(ngdrsfl.o) \
	$(ALIB)(ngdtlst.o)

COBJS = \
	$(ALIB)(ngdgnms.o) \
	$(ALIB)(ngdgtim.o) \
	$(ALIB)(ngdplot.o) \
	$(ALIB)(ngdsatt.o) \
	$(ALIB)(ngdinit.o) \
	$(ALIB)(ngdqatt.o) \
	$(ALIB)(ngdrest.o) \
	$(ALIB)(ngdsave.o)

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

tlink : testngd.f 
	$(LINK.f) testngd.f -o testngd $(ALIB) \
	$(OS_LIB)/gdplot2.a $(OS_LIB)/gdstream.a \
	$(OS_LIB)/gdmap.a  $(OS_LIB)/sfmap.a  \
	$(GEMLIB) $(CGEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) testngd $(OS_BIN)
	$(RM) *.o testngd 

clean:
	$(RM) *.o
