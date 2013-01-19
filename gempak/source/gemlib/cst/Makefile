#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(CGEMLIB)

COBJS = \
	$(ALIB)(cstabbr.o) \
	$(ALIB)(cstalnm.o) \
	$(ALIB)(cstcins.o) \
	$(ALIB)(cstclst.o) \
	$(ALIB)(cstcrnm.o) \
	$(ALIB)(cstctod.o) \
	$(ALIB)(cstctoi.o) \
	$(ALIB)(cstctrl.o) \
	$(ALIB)(cstfind.o) \
	$(ALIB)(cstflst.o) \
	$(ALIB)(cstgtag.o) \
	$(ALIB)(cstilst.o) \
	$(ALIB)(cstinch.o) \
	$(ALIB)(cstitoc.o) \
	$(ALIB)(cstitos.o) \
	$(ALIB)(cstlcuc.o) \
	$(ALIB)(cstldsp.o) \
	$(ALIB)(cstlstr.o) \
	$(ALIB)(cstlstc.o) \
	$(ALIB)(cstncat.o) \
	$(ALIB)(cstncpy.o) \
	$(ALIB)(cstnocc.o) \
	$(ALIB)(cstnumb.o) \
	$(ALIB)(cstopcl.o) \
	$(ALIB)(cstpadstring.o) \
	$(ALIB)(cstptmt.o) \
	$(ALIB)(cstrang.o) \
	$(ALIB)(cstrlch.o) \
	$(ALIB)(cstrlst.o) \
	$(ALIB)(cstrmbl.o) \
	$(ALIB)(cstrmst.o) \
	$(ALIB)(cstrmtag.o) \
	$(ALIB)(cstrnan.o) \
	$(ALIB)(cstrpst.o) \
	$(ALIB)(cstrspc.o) \
	$(ALIB)(cstrxbl.o) \
	$(ALIB)(cstsort.o) \
	$(ALIB)(cstsplit.o) \
	$(ALIB)(cstsrch.o) \
	$(ALIB)(cststag.o) \
	$(ALIB)(cststoi.o) \
	$(ALIB)(csttims.o) \
	$(ALIB)(cstuclc.o) \
	$(ALIB)(cstunpr.o) \
	$(ALIB)(cstwrap.o) \
	$(ALIB)(cstzpad.o)
 
GDIR = cst

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

tlink : test$(GDIR).c
	$(COMPILE.c) test$(GDIR).c
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

