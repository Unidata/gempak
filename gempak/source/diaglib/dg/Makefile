#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(dgadcl.o) \
	$(ALIB)(dgazst.o) \
	$(ALIB)(dgccxgp.o) \
	$(ALIB)(dgcdeg.o) \
	$(ALIB)(dgcfixa.o) \
	$(ALIB)(dgcflno.o) \
	$(ALIB)(dgcget.o) \
	$(ALIB)(dgcglev.o) \
	$(ALIB)(dgcgrid.o) \
	$(ALIB)(dgchck.o) \
	$(ALIB)(dgcinxt.o) \
	$(ALIB)(dgclcn.o) \
	$(ALIB)(dgclmp.o) \
	$(ALIB)(dgclos.o) \
	$(ALIB)(dgcndg.o) \
	$(ALIB)(dgcndtm.o) \
	$(ALIB)(dgcnfil.o) \
	$(ALIB)(dgcnrdt.o) \
	$(ALIB)(dgcntim.o) \
	$(ALIB)(dgcnwdt.o) \
	$(ALIB)(dgcola.o) \
	$(ALIB)(dgcone.o) \
	$(ALIB)(dgcqdtm.o) \
	$(ALIB)(dgcqgrd.o) \
	$(ALIB)(dgcqtms.o) \
	$(ALIB)(dgcset.o) \
	$(ALIB)(dgcsubg.o) \
	$(ALIB)(dgcvecr.o) \
	$(ALIB)(dgcvect.o) \
	$(ALIB)(dgcwbt.o) \
	$(ALIB)(dgdmsf.o) \
	$(ALIB)(dgdriv.o) \
	$(ALIB)(dgdump.o) \
	$(ALIB)(dgdwpt.o) \
	$(ALIB)(dgesub.o) \
	$(ALIB)(dgfall.o) \
	$(ALIB)(dgfclp.o) \
	$(ALIB)(dgfget.o) \
	$(ALIB)(dgfnds.o) \
	$(ALIB)(dgfndv.o) \
	$(ALIB)(dgfrig.o) \
	$(ALIB)(dgg2gc.o) \
	$(ALIB)(dgg2gi.o) \
	$(ALIB)(dgg2gs.o) \
	$(ALIB)(dggetg.o) \
	$(ALIB)(dggetl.o) \
	$(ALIB)(dggets.o) \
	$(ALIB)(dggett.o) \
	$(ALIB)(dggetv.o) \
	$(ALIB)(dggnfl.o) \
	$(ALIB)(dggobs.o) \
	$(ALIB)(dggrdr.o) \
	$(ALIB)(dggrel.o) \
	$(ALIB)(dggrot.o) \
	$(ALIB)(dggtmp.o) \
	$(ALIB)(dggtvl.o) \
	$(ALIB)(dghilo.o) \
	$(ALIB)(dgiget.o) \
	$(ALIB)(dgigrg.o) \
	$(ALIB)(dgintl.o) \
	$(ALIB)(dgiset.o) \
	$(ALIB)(dgkxky.o) \
	$(ALIB)(dglncr.o) \
	$(ALIB)(dglncx.o) \
	$(ALIB)(dgltln.o) \
	$(ALIB)(dgmerr.o) \
	$(ALIB)(dgmhdr.o) \
	$(ALIB)(dgmnam.o) \
	$(ALIB)(dgmscl.o) \
	$(ALIB)(dgmxnt.o) \
	$(ALIB)(dgnend.o) \
	$(ALIB)(dgnewg.o) \
	$(ALIB)(dgnrel.o) \
	$(ALIB)(dgnxts.o) \
	$(ALIB)(dgnxtv.o) \
	$(ALIB)(dgoang.o) \
	$(ALIB)(dgonav.o) \
	$(ALIB)(dgorgn.o) \
	$(ALIB)(dgpfun.o) \
	$(ALIB)(dgprcp.o) \
	$(ALIB)(dgprdr.o) \
	$(ALIB)(dgprft.o) \
	$(ALIB)(dgputs.o) \
	$(ALIB)(dgputv.o) \
	$(ALIB)(dgpwts.o) \
	$(ALIB)(dgqbnd.o) \
	$(ALIB)(dgqkxy.o) \
	$(ALIB)(dgqlyr.o) \
	$(ALIB)(dgqmsl.o) \
	$(ALIB)(dgqref.o) \
	$(ALIB)(dgreal.o) \
	$(ALIB)(dgrgrd.o) \
	$(ALIB)(dgrobs.o) \
	$(ALIB)(dgrpls.o) \
	$(ALIB)(dgrplv.o) \
	$(ALIB)(dgrstm.o) \
	$(ALIB)(dgsare.o) \
	$(ALIB)(dgscal.o) \
	$(ALIB)(dgsnav.o) \
	$(ALIB)(dgsnfl.o) \
	$(ALIB)(dgssub.o) \
	$(ALIB)(dgstlv.o) \
	$(ALIB)(dgt2ig.o) \
	$(ALIB)(dgt2nr.o) \
	$(ALIB)(dgtadc.o) \
	$(ALIB)(dgtemp.o) \
	$(ALIB)(dgtops.o) \
	$(ALIB)(dgtopv.o) \
	$(ALIB)(dgtrig.o) \
	$(ALIB)(dgtrot.o) \
	$(ALIB)(dgtscl.o) \
	$(ALIB)(dgudig.o) \
	$(ALIB)(dgupdh.o) \
	$(ALIB)(dgupdv.o) \
	$(ALIB)(dgupsg.o) \
	$(ALIB)(dgupvg.o) \
	$(ALIB)(dgvcrd.o) \
	$(ALIB)(dgwobs.o)

FOBJS = \
	$(ALIB)(dgcxgp.o) \
	$(ALIB)(dgfixa.o) \
	$(ALIB)(dgflno.o) \
	$(ALIB)(dgglev.o) \
	$(ALIB)(dggrid.o) \
	$(ALIB)(dggridn.o) \
	$(ALIB)(dginxt.o) \
	$(ALIB)(dgndtm.o) \
	$(ALIB)(dgnfil.o) \
	$(ALIB)(dgnrdt.o) \
	$(ALIB)(dgntim.o) \
	$(ALIB)(dgnwdt.o) \
	$(ALIB)(dgqdtm.o) \
	$(ALIB)(dgqgrd.o) \
	$(ALIB)(dgqtms.o) \
	$(ALIB)(dgsubg.o) \
	$(ALIB)(dgsubgn.o) \
	$(ALIB)(dgvecr.o) \
	$(ALIB)(dgvecrn.o) \
	$(ALIB)(dgvect.o) \
	$(ALIB)(dgvectn.o)
 
GDIR = dg

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
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(CGEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

