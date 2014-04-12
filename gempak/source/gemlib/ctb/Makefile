#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(CGEMLIB)

COBJS = \
	$(ALIB)(ctbafos.o) \
	$(ALIB)(ctbairmet.o) \
	$(ALIB)(ctbastn.o) \
	$(ALIB)(ctbawdef.o) \
	$(ALIB)(ctbawmap.o) \
	$(ALIB)(ctbccfind.o) \
	$(ALIB)(ctbccrd.o) \
	$(ALIB)(ctbdcatitos.o) \
	$(ALIB)(ctbdcatstoi.o) \
	$(ALIB)(ctbdhrsitos.o) \
	$(ALIB)(ctbdhrsstoi.o) \
	$(ALIB)(ctbdscatitos.o) \
	$(ALIB)(ctbdscatstoi.o) \
	$(ALIB)(ctbdtcat.o) \
	$(ALIB)(ctbdtdump.o) \
	$(ALIB)(ctbdtget.o) \
	$(ALIB)(ctbdtlist.o) \
	$(ALIB)(ctbdtpath.o) \
	$(ALIB)(ctbdtrd.o) \
	$(ALIB)(ctbdttime.o) \
	$(ALIB)(ctbdttmpl.o) \
	$(ALIB)(ctbfszfnd.o) \
	$(ALIB)(ctbfsznam.o) \
	$(ALIB)(ctbfszqn.o) \
	$(ALIB)(ctbfszrd.o) \
	$(ALIB)(ctbfszval.o) \
	$(ALIB)(ctbfszxvl.o) \
	$(ALIB)(ctbg2gnam.o) \
	$(ALIB)(ctbg2gnum.o) \
	$(ALIB)(ctbg2rdcntr.o) \
	$(ALIB)(ctbg2rdlvl.o) \
	$(ALIB)(ctbg2rdvar.o) \
	$(ALIB)(ctbg2read.o) \
	$(ALIB)(ctbhfdump.o) \
	$(ALIB)(ctbhfgetfont.o) \
	$(ALIB)(ctbhfread.o) \
	$(ALIB)(ctblygetcmode.o) \
	$(ALIB)(ctblygetcolor.o) \
	$(ALIB)(ctblygetdsply.o) \
	$(ALIB)(ctblygetfile.o) \
	$(ALIB)(ctblygetfmode.o) \
	$(ALIB)(ctblygetgrptyp.o) \
	$(ALIB)(ctblygetname.o) \
	$(ALIB)(ctblygetoutfile.o) \
	$(ALIB)(ctblyrd.o) \
	$(ALIB)(ctbmzgnm.o) \
	$(ALIB)(ctbmzrd.o) \
	$(ALIB)(ctbpermccfind.o) \
	$(ALIB)(ctbpermccrd.o) \
	$(ALIB)(ctbpfbool.o) \
	$(ALIB)(ctbpfread.o) \
	$(ALIB)(ctbpfstr.o) \
	$(ALIB)(ctbpldump.o) \
	$(ALIB)(ctbplgcc.o) \
	$(ALIB)(ctbplget.o) \
	$(ALIB)(ctbpllist.o) \
	$(ALIB)(ctbplrd.o) \
	$(ALIB)(ctbplsdef.o) \
	$(ALIB)(ctbplset.o) \
	$(ALIB)(ctbprmt.o) \
	$(ALIB)(ctbprod.o) \
	$(ALIB)(ctbrbul.o) \
	$(ALIB)(ctbrdcpf.o) \
	$(ALIB)(ctbrdprf.o) \
	$(ALIB)(ctbrdtyp.o) \
	$(ALIB)(ctbrmtyp.o) \
	$(ALIB)(ctbrstn.o) \
	$(ALIB)(ctbtiff.o) \
	$(ALIB)(ctbtrkfnd.o) \
	$(ALIB)(ctbtrkitv.o) \
	$(ALIB)(ctbtrkqn.o) \
	$(ALIB)(ctbwrcpf.o)


GDIR = ctb

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS)
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

