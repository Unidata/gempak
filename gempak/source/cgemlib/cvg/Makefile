#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(CGEMLIB)

PROGS = convcvg dumpvgf

COBJS = \
	$(ALIB)(cvgallocgfablock.o) \
	$(ALIB)(cvgc3e3f.o) \
	$(ALIB)(cvgc3i3h.o) \
	$(ALIB)(cvgc3k3j.o) \
	$(ALIB)(cvgcheckplace.o) \
	$(ALIB)(cvgcir2lin.o) \
	$(ALIB)(cvgclearplace.o) \
	$(ALIB)(cvgclos.o) \
	$(ALIB)(cvgcp.o) \
	$(ALIB)(cvgcrall.o) \
	$(ALIB)(cvgcrvgf.o) \
	$(ALIB)(cvgdeall.o) \
	$(ALIB)(cvgdelet.o) \
	$(ALIB)(cvgdrawlayer.o) \
	$(ALIB)(cvgdump.o) \
	$(ALIB)(cvgel2hndl.o) \
	$(ALIB)(cvgel2place.o) \
	$(ALIB)(cvgeladj.o) \
	$(ALIB)(cvgfreebkpts.o) \
	$(ALIB)(cvgfreeelptr.o) \
	$(ALIB)(cvgfreetc.o) \
	$(ALIB)(cvgfscan.o) \
	$(ALIB)(cvggetelfilter.o) \
	$(ALIB)(cvggetfilter.o) \
	$(ALIB)(cvggetfld.o) \
	$(ALIB)(cvggetflghtlvl.o) \
	$(ALIB)(cvggetfname.o) \
	$(ALIB)(cvggetoutdir.o) \
	$(ALIB)(cvggettblfilter.o) \
	$(ALIB)(cvggetworkfile.o) \
	$(ALIB)(cvggfrmtrx.o) \
	$(ALIB)(cvggmtrx.o) \
	$(ALIB)(cvggtgnum.o) \
	$(ALIB)(cvggtnumpts.o) \
	$(ALIB)(cvginitelm.o) \
	$(ALIB)(cvginitplace.o) \
	$(ALIB)(cvglayer4place.o) \
	$(ALIB)(cvgld4place.o) \
	$(ALIB)(cvglevel.o) \
	$(ALIB)(cvgload.o) \
	$(ALIB)(cvgload2.o) \
	$(ALIB)(cvgmatchfilter.o) \
	$(ALIB)(cvgoldswap.o) \
	$(ALIB)(cvgopen.o) \
	$(ALIB)(cvgopenj.o) \
	$(ALIB)(cvgplaced.o) \
	$(ALIB)(cvgplenabled.o) \
	$(ALIB)(cvgplrtbl.o) \
	$(ALIB)(cvgqkopen.o) \
	$(ALIB)(cvgrdele.o) \
	$(ALIB)(cvgrdfilter.o) \
	$(ALIB)(cvgrdgfa.o) \
	$(ALIB)(cvgrdgtn.o) \
	$(ALIB)(cvgrdhdr.o) \
	$(ALIB)(cvgrdjele.o) \
	$(ALIB)(cvgrdjhdr.o) \
	$(ALIB)(cvgrdjrecnoc.o) \
	$(ALIB)(cvgrdoldele.o) \
	$(ALIB)(cvgrdrec.o) \
	$(ALIB)(cvgrdrecnoc.o) \
	$(ALIB)(cvgrdsel.o) \
	$(ALIB)(cvgrdtc.o) \
	$(ALIB)(cvgrdtca.o) \
	$(ALIB)(cvgrebuildplace.o) \
	$(ALIB)(cvgrebun.o) \
	$(ALIB)(cvgredraw.o) \
	$(ALIB)(cvgrfrsh.o) \
	$(ALIB)(cvgrfrshlayer.o) \
	$(ALIB)(cvgrmfld.o) \
	$(ALIB)(cvgrndef.o) \
	$(ALIB)(cvgscan.o) \
	$(ALIB)(cvgscangfa.o) \
	$(ALIB)(cvgsetfilter.o) \
	$(ALIB)(cvgsetfld.o) \
	$(ALIB)(cvgsetginf.o) \
	$(ALIB)(cvgsetsubtyp.o) \
	$(ALIB)(cvgsetupplace.o) \
	$(ALIB)(cvgsnapjet.o) \
	$(ALIB)(cvgsrchgrp.o) \
	$(ALIB)(cvgsubtyp.o) \
	$(ALIB)(cvgsvfhed.o) \
	$(ALIB)(cvgswap.o) \
	$(ALIB)(cvgt2v.o) \
	$(ALIB)(cvgtodev.o) \
	$(ALIB)(cvgtodev2.o) \
	$(ALIB)(cvgundel.o) \
	$(ALIB)(cvgupdateplace.o) \
	$(ALIB)(cvgv2t.o) \
	$(ALIB)(cvgv2x.o) \
	$(ALIB)(cvgvalid.o) \
	$(ALIB)(cvgwrite.o) \
	$(ALIB)(cvgwriteD.o) \
	$(ALIB)(cvgwritef.o) \
	$(ALIB)(cvgwritefD.o) \
	$(ALIB)(cvgwritgfa.o) \
	$(ALIB)(cvgwrittc.o) \
	$(ALIB)(cvgwrittca.o) \
	$(ALIB)(compareVgf.o) \
	$(ALIB)(ggdvgf.o) \
	$(ALIB)(ctbgfa.o)

GDIR = cvg

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<



$(PROGS): $(ALIB) $(GEMLIB)
	$(COMPILE.c) $@.c
	$(LINK.f) $(NOFORMAIN) -o $@ $@.o $(CGEMLIB) $(GEMLIB) $(APPL) $(GEMLIB) $(CGEMLIB) $(GEMLIB) $(SYSLIB) -lm
	$(RM) $@.o

install: $(PROGS)
	$(INSTALL) convcvg $(OS_BIN)
	$(INSTALL) dumpvgf $(OS_BIN)

clean:
	$(RM) *.o $(PROGS)

tlink : test$(GDIR).c
	$(COMPILE.c) test$(GDIR).c
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(CGEMLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

