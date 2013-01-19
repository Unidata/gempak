#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(incatmap.o) \
	$(ALIB)(incoutt.o) \
	$(ALIB)(incpccftxt.o) \
	$(ALIB)(incpgfatxt.o) \
	$(ALIB)(indiscrete.o) \
	$(ALIB)(indlines.o)

FOBJS = \
	$(ALIB)(inaxis.o) \
	$(ALIB)(inbdta.o) \
	$(ALIB)(incbar.o) \
	$(ALIB)(incclr.o) \
	$(ALIB)(inccol.o) \
	$(ALIB)(incint.o) \
	$(ALIB)(incldt.o) \
	$(ALIB)(incolr.o) \
	$(ALIB)(incont.o) \
	$(ALIB)(inctyp.o) \
	$(ALIB)(inedge.o) \
	$(ALIB)(infile.o) \
	$(ALIB)(infill.o) \
	$(ALIB)(infilt.o) \
	$(ALIB)(ingskp.o) \
	$(ALIB)(inhilo.o) \
	$(ALIB)(inhlsy.o) \
	$(ALIB)(inicng.o) \
	$(ALIB)(inintc.o) \
	$(ALIB)(inline.o) \
	$(ALIB)(inmark.o) \
	$(ALIB)(inmscl.o) \
	$(ALIB)(inmrgd.o) \
	$(ALIB)(inmrkr.o) \
	$(ALIB)(innumb.o) \
	$(ALIB)(inoutt.o) \
	$(ALIB)(inparm.o) \
	$(ALIB)(inprmc.o) \
	$(ALIB)(inprmf.o) \
	$(ALIB)(inptnd.o) \
	$(ALIB)(inptyp.o) \
	$(ALIB)(inpwnd.o) \
	$(ALIB)(inpwth.o) \
	$(ALIB)(inrvec.o) \
	$(ALIB)(inscal.o) \
	$(ALIB)(inside.o) \
	$(ALIB)(inskip.o) \
	$(ALIB)(inskyc.o) \
	$(ALIB)(instyp.o) \
	$(ALIB)(intaxs.o) \
	$(ALIB)(intext.o) \
	$(ALIB)(intitl.o) \
	$(ALIB)(inturb.o) \
	$(ALIB)(intxtn.o) \
	$(ALIB)(intype.o) \
	$(ALIB)(inwind.o) \
	$(ALIB)(inwsym.o)
 
GDIR = in

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS) $(COBJS)
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

