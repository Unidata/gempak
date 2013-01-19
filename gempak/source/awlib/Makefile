#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(awrfil.o)

FOBJS = \
	$(ALIB)(aw2clg.o) \
	$(ALIB)(aw2cst.o) \
	$(ALIB)(awachr.o) \
	$(ALIB)(awadbt.o) \
	$(ALIB)(awalph.o) \
	$(ALIB)(awaplt.o) \
	$(ALIB)(awgabv.o) \
	$(ALIB)(awgcuv.o) \
	$(ALIB)(awgdef.o) \
	$(ALIB)(awglsv.o) \
	$(ALIB)(awgrph.o) \
	$(ALIB)(awpend.o) \
	$(ALIB)(awpidb.o) \
	$(ALIB)(awpinf.o) \
	$(ALIB)(awplin.o) \
	$(ALIB)(awpmap.o) \
	$(ALIB)(awpplt.o) \
	$(ALIB)(awprod.o) \
	$(ALIB)(awread.o) \
	$(ALIB)(awscom.o) \
	$(ALIB)(awsdef.o) \
	$(ALIB)(awsver.o) \
	$(ALIB)(awsyst.o) \
	$(ALIB)(awwxsm.o)
 
GDIR = aw

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o


.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


install:


clean:
	$(RM) *.o 

tlink : test$(GDIR).f 
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

