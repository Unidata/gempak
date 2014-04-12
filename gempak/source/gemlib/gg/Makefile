#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(ggosrd.o) \
	$(ALIB)(ggqbrd.o) \
	$(ALIB)(ggqsrd.o) \
	$(ALIB)(ggrtrk.o) \
	$(ALIB)(ggwcpb.o) \
	$(ALIB)(ggwcvf.o) \
	$(ALIB)(ggwlso.o) \
	$(ALIB)(ggrwsp.o)

FOBJS = \
	$(ALIB)(ggairm.o) \
	$(ALIB)(ggalis.o) \
	$(ALIB)(ggarc.o) \
	$(ALIB)(ggasaw.o) \
	$(ALIB)(ggasct.o) \
	$(ALIB)(ggasdi.o) \
	$(ALIB)(ggasdip.o) \
	$(ALIB)(ggasdiqc.o) \
	$(ALIB)(ggasrd.o) \
	$(ALIB)(ggbnd.o) \
	$(ALIB)(ggbox.o) \
	$(ALIB)(ggcbar.o) \
	$(ALIB)(ggcpnl.o) \
	$(ALIB)(ggcsig.o) \
	$(ALIB)(ggctrd.o) \
        $(ALIB)(ggcwcp.o) \
	$(ALIB)(ggdltn.o) \
	$(ALIB)(ggdmap.o) \
	$(ALIB)(ggency.o) \
	$(ALIB)(ggedr.o) \
	$(ALIB)(ggedrp.o) \
	$(ALIB)(ggfaxs.o) \
	$(ALIB)(ggffa.o) \
	$(ALIB)(gggairm.o) \
	$(ALIB)(gghrcn.o) \
	$(ALIB)(gginfo.o) \
	$(ALIB)(gginit.o) \
	$(ALIB)(ggisig.o) \
	$(ALIB)(gglnup.o) \
	$(ALIB)(ggltln.o) \
	$(ALIB)(ggltng.o) \
	$(ALIB)(ggltrp.o) \
	$(ALIB)(ggmap.o) \
	$(ALIB)(ggmaps.o) \
	$(ALIB)(ggmisc.o) \
	$(ALIB)(ggmotf.o) \
	$(ALIB)(ggnarp.o) \
	$(ALIB)(ggnatc.o) \
	$(ALIB)(ggncon.o) \
	$(ALIB)(ggosct.o) \
	$(ALIB)(ggpanl.o) \
	$(ALIB)(ggproj.o) \
	$(ALIB)(ggqsct.o) \
	$(ALIB)(ggrvec.o) \
	$(ALIB)(ggrwav.o) \
	$(ALIB)(ggscal.o) \
	$(ALIB)(ggsdev.o) \
	$(ALIB)(ggsgrf.o) \
	$(ALIB)(ggskew.o) \
	$(ALIB)(ggsplt.o) \
	$(ALIB)(ggsttl.o) \
	$(ALIB)(ggtcda.o) \
	$(ALIB)(ggtcep.o) \
	$(ALIB)(ggtclb.o) \
	$(ALIB)(ggtcmg.o) \
	$(ALIB)(ggtcsh.o) \
	$(ALIB)(ggtcsm.o) \
	$(ALIB)(ggtctb.o) \
	$(ALIB)(ggtlbl.o) \
	$(ALIB)(ggtrak.o) \
	$(ALIB)(ggwact.o) \
	$(ALIB)(ggwarn.o) \
	$(ALIB)(ggwave.o) \
	$(ALIB)(ggwcck.o) \
	$(ALIB)(ggwcp.o) \
	$(ALIB)(ggwcur.o) \
	$(ALIB)(ggwlbl.o) \
	$(ALIB)(ggwnck.o) \
	$(ALIB)(ggwplt.o) \
	$(ALIB)(ggwsat.o) \
	$(ALIB)(ggwsrd.o) \
	$(ALIB)(ggwstm.o) \
	$(ALIB)(ggwspd.o) \
	$(ALIB)(ggwstr.o) \
	$(ALIB)(ggwtch.o) \
	$(ALIB)(ggwusc.o) \
	$(ALIB)(ggwfps.o) \
	$(ALIB)(ggwwcn.o) \
	$(ALIB)(ggwwcr.o) \
	$(ALIB)(ggwwfo.o) \
	$(ALIB)(ggwwou.o) \
	$(ALIB)(ggwwtp.o) \
	$(ALIB)(ggzare.o) \
	$(ALIB)(ggzsrt.o)
 
GDIR = gg

#
# add bufr flags for ggqbrd (use -DDEBUG to output melbufr messages)
CFLAGS = $(COPT) $(BUFRFLAGS)

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
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(OS_LIB)/textlib.a $(CGEMLIB) $(APPL) $(GPLT) $(SYSLIB) $(GEMLIB) $(OS_LIB)/libgpc.a $(NETCDF) $(ZLIB) $(RSLLIB) $(LIBBZ2) $(OS_LIB)/libmel_bufr.a
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)
