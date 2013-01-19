#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = 

COBJS = \
	$(ALIB)(dfabs.o) \
	$(ALIB)(dfacos.o) \
	$(ALIB)(dfadd.o) \
	$(ALIB)(dfand.o) \
	$(ALIB)(dfasin.o) \
	$(ALIB)(dfatan.o) \
	$(ALIB)(dfatn2.o) \
	$(ALIB)(dfavg.o) \
	$(ALIB)(dfbeta.o) \
	$(ALIB)(dfbool.o) \
	$(ALIB)(dfbncdf.o) \
	$(ALIB)(dfbvsq.o) \
	$(ALIB)(dfcorl.o) \
	$(ALIB)(dfcos.o) \
	$(ALIB)(dfdden.o) \
	$(ALIB)(dfddt.o) \
	$(ALIB)(dfddx.o) \
	$(ALIB)(dfddy.o) \
	$(ALIB)(dfeor.o) \
	$(ALIB)(dfeq.o) \
	$(ALIB)(dfexp.o) \
	$(ALIB)(dfexpi.o) \
	$(ALIB)(dffosb.o) \
	$(ALIB)(dfge.o) \
	$(ALIB)(dfgele.o) \
	$(ALIB)(dfgelt.o) \
	$(ALIB)(dfgt.o) \
	$(ALIB)(dfgtle.o) \
	$(ALIB)(dfgtlt.o) \
	$(ALIB)(dfgwfs.o) \
	$(ALIB)(dfhigh.o) \
	$(ALIB)(dfhilo.o) \
	$(ALIB)(dfibncd.o) \
	$(ALIB)(dfigpt.o) \
	$(ALIB)(dfint.o) \
	$(ALIB)(dfincd.o) \
	$(ALIB)(dfjcbn.o) \
	$(ALIB)(dfjgpt.o) \
	$(ALIB)(dfknts.o) \
	$(ALIB)(dflap.o) \
	$(ALIB)(dflav.o) \
	$(ALIB)(dfldf.o) \
	$(ALIB)(dfle.o) \
	$(ALIB)(dfln.o) \
	$(ALIB)(dflog.o) \
	$(ALIB)(dflows.o) \
	$(ALIB)(dflt.o) \
	$(ALIB)(dfmask.o) \
	$(ALIB)(dfmass.o) \
	$(ALIB)(dfmiss.o) \
	$(ALIB)(dfmixr.o) \
	$(ALIB)(dfmul.o) \
	$(ALIB)(dfncdf.o) \
	$(ALIB)(dfne.o) \
	$(ALIB)(dfnint.o) \
	$(ALIB)(dfnot.o) \
	$(ALIB)(dfor.o) \
	$(ALIB)(dfpaub.o) \
	$(ALIB)(dfplcl.o) \
	$(ALIB)(dfprcl.o) \
	$(ALIB)(dfpois.o) \
	$(ALIB)(dfquo.o) \
	$(ALIB)(dfrelh.o) \
	$(ALIB)(dfreli.o) \
	$(ALIB)(dfsavs.o) \
	$(ALIB)(dfsbtw.o) \
	$(ALIB)(dfsge.o) \
	$(ALIB)(dfsgmn.o) \
	$(ALIB)(dfsgmx.o) \
	$(ALIB)(dfsgt.o) \
	$(ALIB)(dfsin.o) \
	$(ALIB)(dfsle.o) \
	$(ALIB)(dfslt.o) \
	$(ALIB)(dfsm5s.o) \
	$(ALIB)(dfsm9s.o) \
	$(ALIB)(dfsmax.o) \
	$(ALIB)(dfsmin.o) \
	$(ALIB)(dfsqrt.o) \
	$(ALIB)(dfstab.o) \
	$(ALIB)(dfsub.o) \
	$(ALIB)(dftan.o) \
	$(ALIB)(dftav.o) \
	$(ALIB)(dftdf.o) \
	$(ALIB)(dfthan.o) \
	$(ALIB)(dfthes.o) \
	$(ALIB)(dfthta.o) \
	$(ALIB)(dfthte.o) \
	$(ALIB)(dfthwc.o) \
	$(ALIB)(dftlcl.o) \
	$(ALIB)(dftmst.o) \
	$(ALIB)(dftmwk.o) \
	$(ALIB)(dfwndx.o) \
	$(ALIB)(dfxav.o) \
	$(ALIB)(dfxsum.o) \
	$(ALIB)(dfxval.o) \
	$(ALIB)(dfyav.o) \
	$(ALIB)(dfysum.o) \
	$(ALIB)(dfyval.o)
 
GDIR = df

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(COBJS) $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<



clean:
	$(RM) *.o 

tlink : test$(GDIR).f 
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)
