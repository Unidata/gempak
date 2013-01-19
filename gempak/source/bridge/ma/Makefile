#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(maadjt.o) \
	$(ALIB)(mabst0.o) \
	$(ALIB)(mabtyp.o) \
	$(ALIB)(macgar.o) \
	$(ALIB)(macgbg.o) \
	$(ALIB)(macgck.o) \
	$(ALIB)(macgcl.o) \
	$(ALIB)(macgdc.o) \
	$(ALIB)(macgdt.o) \
	$(ALIB)(macggr.o) \
	$(ALIB)(macgid.o) \
	$(ALIB)(macgin.o) \
	$(ALIB)(macgmm.o) \
	$(ALIB)(macgpr.o) \
	$(ALIB)(macgpt.o) \
	$(ALIB)(macgrm.o) \
	$(ALIB)(macgsc.o) \
	$(ALIB)(macgsl.o) \
	$(ALIB)(macgst.o) \
	$(ALIB)(macgvs.o) \
	$(ALIB)(macgwa.o) \
	$(ALIB)(macgwd.o) \
	$(ALIB)(macgwn.o) \
	$(ALIB)(macgws.o) \
	$(ALIB)(macgwv.o) \
	$(ALIB)(macgwx.o) \
	$(ALIB)(mackrp.o) \
	$(ALIB)(macld1.o) \
	$(ALIB)(maclds.o) \
	$(ALIB)(macst0.o) \
	$(ALIB)(macwnd.o) \
	$(ALIB)(mad0qc.o) \
	$(ALIB)(madcd1.o) \
	$(ALIB)(madcd2.o) \
	$(ALIB)(madcod.o) \
	$(ALIB)(maddff.o) \
	$(ALIB)(mads4q.o) \
	$(ALIB)(mads4r.o) \
	$(ALIB)(madst0.o) \
	$(ALIB)(madsvs.o) \
	$(ALIB)(madwph.o) \
	$(ALIB)(maelev.o) \
	$(ALIB)(maerrs.o) \
	$(ALIB)(mafind.o) \
	$(ALIB)(magemp.o) \
	$(ALIB)(magrpt.o) \
	$(ALIB)(maicea.o) \
	$(ALIB)(maifpt.o) \
	$(ALIB)(maifsp.o) \
	$(ALIB)(maiihv.o) \
	$(ALIB)(mainit.o) \
	$(ALIB)(maintf.o) \
	$(ALIB)(malscb.o) \
	$(ALIB)(malscd.o) \
	$(ALIB)(mamawx.o) \
	$(ALIB)(manddf.o) \
	$(ALIB)(maobst.o) \
	$(ALIB)(mapkwd.o) \
	$(ALIB)(mapr24.o) \
	$(ALIB)(maprec.o) \
	$(ALIB)(mapres.o) \
	$(ALIB)(maprs3.o) \
	$(ALIB)(marelh.o) \
	$(ALIB)(mas4dg.o) \
	$(ALIB)(mas4eg.o) \
	$(ALIB)(mas4ll.o) \
	$(ALIB)(mas4tp.o) \
	$(ALIB)(masc1b.o) \
	$(ALIB)(masc1d.o) \
	$(ALIB)(masc2d.o) \
	$(ALIB)(masc3d.o) \
	$(ALIB)(masc4d.o) \
	$(ALIB)(masec2.o) \
	$(ALIB)(masec3.o) \
	$(ALIB)(masec5.o) \
	$(ALIB)(maship.o) \
	$(ALIB)(mastbl.o) \
	$(ALIB)(maswld.o) \
	$(ALIB)(matdur.o) \
	$(ALIB)(matemp.o) \
	$(ALIB)(matesa.o) \
	$(ALIB)(mawgem.o) \
	$(ALIB)(mawspd.o) \
	$(ALIB)(mawvh1.o) \
	$(ALIB)(mawvph.o)
 
GDIR = ma

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

