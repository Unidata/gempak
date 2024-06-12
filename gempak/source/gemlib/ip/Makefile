#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(rlgets.o) \
	$(ALIB)(ipread.o) \
	$(ALIB)(ipcwin.o) \
	$(ALIB)(ipdflt.o) \
	$(ALIB)(ipdynm.o) \
	$(ALIB)(ipexit.o) \
	$(ALIB)(ipfvar.o) \
	$(ALIB)(ipgsav.o) \
	$(ALIB)(ipgtpt.o) \
	$(ALIB)(iphelp.o) \
	$(ALIB)(ipidnt.o) \
	$(ALIB)(ipinit.o) \
	$(ALIB)(iplctl.o) \
	$(ALIB)(iplist.o) \
	$(ALIB)(iplog.o) \
	$(ALIB)(iplutf.o) \
	$(ALIB)(ipmfil.o) \
	$(ALIB)(ipputv.o) \
	$(ALIB)(ipresp.o) \
	$(ALIB)(iprest.o) \
	$(ALIB)(iprqst.o) \
	$(ALIB)(ipsave.o) \
	$(ALIB)(ipsavf.o) \
	$(ALIB)(ipstr.o) \
	$(ALIB)(ipstrp.o) \
	$(ALIB)(ipsvar.o) \
	$(ALIB)(ipswin.o) \
	$(ALIB)(ipvers.o)

GDIR = ip

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -J$(OS_INC) -c $<


clean:
	$(RM) *.o 

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

