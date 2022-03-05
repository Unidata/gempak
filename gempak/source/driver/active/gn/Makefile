#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB =	$(OS_LIB)/gn.a

COBJS = \
	$(ALIB)(pngread.o) \
	$(ALIB)(cpgdmpct.o) \
	$(ALIB)(cpgfindstart.o) \
	$(ALIB)(cpgfindstop.o) \
	$(ALIB)(cpggetfile.o) \
	$(ALIB)(cpggetfname.o) \
	$(ALIB)(cpggetsix.o) \
	$(ALIB)(cpgmtch.o) \
	$(ALIB)(cpgprint.o) \
	$(ALIB)(cpgprse.o) \
	$(ALIB)(cpgrdhdr.o) \
	$(ALIB)(cpgreadcuts.o) \
	$(ALIB)(cpgrindex.o) \
	$(ALIB)(cpgrlst.o) \
	$(ALIB)(cpgsetbits.o) \
	$(ALIB)(cpgshoct.o) \
	$(ALIB)(cpgsixrd.o) \
	$(ALIB)(cpgxplane.o)

PROG = gn
 
DEVLIBS = 
 
LOBJS = \
	$(ALIB)(cctabl.o) \
	$(ALIB)(cqcomp.o) \
	$(ALIB)(crarea.o) \
	$(ALIB)(crastr.o) \
	$(ALIB)(crgini.o) \
	$(ALIB)(crncdf.o) \
	$(ALIB)(crnexbz.o) \
	$(ALIB)(crnexz.o) \
	$(ALIB)(crnex2.o) \
	$(ALIB)(crnfax.o) \
	$(ALIB)(crnids.o) \
	$(ALIB)(crnowr.o) \
	$(ALIB)(cscnam.o) \
	$(ALIB)(cscrgb.o) \
	$(ALIB)(csctbl.o) \
	$(ALIB)(csinit.o) \
	$(ALIB)(hcirc.o) \
	$(ALIB)(hclear.o) \
	$(ALIB)(hclosp.o) \
	$(ALIB)(hclpnl.o) \
	$(ALIB)(hegrp.o) \
	$(ALIB)(henanm.o) \
	$(ALIB)(hendd.o) \
	$(ALIB)(heplot.o) \
	$(ALIB)(hevent.o) \
	$(ALIB)(hfill.o) \
	$(ALIB)(hfrnt.o) \
	$(ALIB)(hgsave.o) \
	$(ALIB)(hgtpnt.o) \
	$(ALIB)(hhash.o) \
	$(ALIB)(hinit.o) \
	$(ALIB)(hinita.o) \
	$(ALIB)(hinitp.o) \
	$(ALIB)(hline.o) \
	$(ALIB)(hlogo.o) \
	$(ALIB)(hloopc.o) \
	$(ALIB)(hmark.o) \
	$(ALIB)(hmesg.o) \
	$(ALIB)(hopen.o) \
	$(ALIB)(hqclrs.o) \
	$(ALIB)(hqcomp.o) \
	$(ALIB)(hqdatt.o) \
	$(ALIB)(hrfill.o) \
	$(ALIB)(hrline.o) \
	$(ALIB)(hroam.o) \
	$(ALIB)(hrspln.o) \
	$(ALIB)(hsatim.o) \
	$(ALIB)(hsatpx.o) \
	$(ALIB)(hsbrgb.o) \
	$(ALIB)(hscint.o) \
	$(ALIB)(hsclr2.o) \
	$(ALIB)(hscnam.o) \
	$(ALIB)(hscolb.o) \
	$(ALIB)(hscolr.o) \
	$(ALIB)(hscrgb.o) \
	$(ALIB)(hsctbl.o) \
	$(ALIB)(hsdash.o) \
	$(ALIB)(hsdatt.o) \
	$(ALIB)(hsfill.o) \
	$(ALIB)(hsfrnt.o) \
	$(ALIB)(hsgrp.o) \
	$(ALIB)(hsgtgn.o) \
	$(ALIB)(hshash.o) \
	$(ALIB)(hsicmn.o) \
	$(ALIB)(hsline.o) \
	$(ALIB)(hsltyp.o) \
	$(ALIB)(hslwid.o) \
	$(ALIB)(hslwin.o) \
	$(ALIB)(hsmark.o) \
	$(ALIB)(hsmrkr.o) \
	$(ALIB)(hsplot.o) \
	$(ALIB)(hsroam.o) \
	$(ALIB)(hssmth.o) \
	$(ALIB)(hsspln.o) \
	$(ALIB)(hssymb.o) \
	$(ALIB)(hstanm.o) \
	$(ALIB)(hstext.o) \
	$(ALIB)(hswind.o) \
	$(ALIB)(hsymb.o) \
	$(ALIB)(htext.o) \
	$(ALIB)(htextc.o) \
	$(ALIB)(htxsy.o) \
	$(ALIB)(hwind.o)

LIBINC = \
	$(GEMLIB) \
	$(DEVICE) \
	$(ALIB) \
	$(DEVLIBS) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(SYSLIB) \
	-lm

GDIR = gn

.PRECIOUS: $(ALIB)

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC) -I$(NETCDFINC) -I$(ZLIBINC) -I$(PNGINC) -I$(RSLINC)

all : $(ALIB) $(PROG)

install:
	$(INSTALL) $(PROG) $(OS_BIN)

$(PROG): $(ALIB)
	$(LINK.f) device.f $(LIBINC) -o $@  
	$(RM) device.o

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -g -c $<

.f.a :
	$(FC) $(FFLAGS) -g -c $<


clean:
	$(RM) *.o $(PROG)

