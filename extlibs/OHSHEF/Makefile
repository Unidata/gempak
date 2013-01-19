#
#	Makefile for GEMPAK 6.0
#		Builds libaodt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/libOHSHEF.a

LOBJS = \
	$(ALIB)(sh2adj.o) \
	$(ALIB)(sh2beg.o) \
	$(ALIB)(sh2bla.o) \
	$(ALIB)(shcurd.o) \
	$(ALIB)(sh2dt2.o) \
	$(ALIB)(sh2fnd.o) \
	$(ALIB)(sh2loc.o) \
	$(ALIB)(sh2nu2.o) \
	$(ALIB)(sh2num.o) \
	$(ALIB)(sh2nxd.o) \
	$(ALIB)(sh2skp.o) \
	$(ALIB)(sh2st1.o) \
	$(ALIB)(sh2st2.o) \
	$(ALIB)(sh2tya.o) \
	$(ALIB)(sh2tyd.o) \
	$(ALIB)(sh3ad7.o) \
	$(ALIB)(sh3add.o) \
	$(ALIB)(sh3adh.o) \
	$(ALIB)(sh3adk.o) \
	$(ALIB)(sh3adt.o) \
	$(ALIB)(sh3ady.o) \
	$(ALIB)(sh3dec.o) \
	$(ALIB)(sh3dlm.o) \
	$(ALIB)(sh3dt0.o) \
	$(ALIB)(sh3dt1.o) \
	$(ALIB)(sh3dt2.o) \
	$(ALIB)(sh3dt3.o) \
	$(ALIB)(sh3dt4.o) \
	$(ALIB)(sh3dt5.o) \
	$(ALIB)(sh3dt6.o) \
	$(ALIB)(sh3dt7.o) \
	$(ALIB)(sh3dt8.o) \
	$(ALIB)(sh3tz0.o) \
	$(ALIB)(sh4dt0.o) \
	$(ALIB)(sh4dt2.o) \
	$(ALIB)(shcdat.o) \
	$(ALIB)(shctim.o) \
	$(ALIB)(shdble.o) \
	$(ALIB)(shdcod.o) \
	$(ALIB)(shdeca.o) \
	$(ALIB)(shdecb.o) \
	$(ALIB)(shdece.o) \
	$(ALIB)(shdotb.o) \
	$(ALIB)(shdriv.o) \
	$(ALIB)(shdura.o) \
	$(ALIB)(shedln.o) \
	$(ALIB)(sherr.o) \
	$(ALIB)(sherrk.o) \
	$(ALIB)(sherrm.o) \
	$(ALIB)(sherrs.o) \
	$(ALIB)(shexcd.o) \
	$(ALIB)(shfact.o) \
	$(ALIB)(shfmt1.o) \
	$(ALIB)(shfmt2.o) \
	$(ALIB)(shgcal.o) \
	$(ALIB)(shgetk.o) \
	$(ALIB)(shgjul.o) \
	$(ALIB)(shhrfx.o) \
	$(ALIB)(shieom.o) \
	$(ALIB)(shihr.o) \
	$(ALIB)(shijul.o) \
	$(ALIB)(shimn.o) \
	$(ALIB)(shimo.o) \
	$(ALIB)(shise.o) \
	$(ALIB)(shiyr.o) \
	$(ALIB)(shline.o) \
	$(ALIB)(shmaxe.o) \
	$(ALIB)(shout.o) \
	$(ALIB)(shoza1.o) \
	$(ALIB)(shoze1.o) \
	$(ALIB)(shoze2.o) \
	$(ALIB)(shpabg.o) \
	$(ALIB)(shpcod.o) \
	$(ALIB)(shpdec.o) \
	$(ALIB)(shpos.o) \
	$(ALIB)(shprob.o) \
	$(ALIB)(shqual.o) \
	$(ALIB)(shquot.o) \
	$(ALIB)(shsava.o) \
	$(ALIB)(shsavd.o) \
	$(ALIB)(shsavi.o) \
	$(ALIB)(shsavj.o) \
	$(ALIB)(shsavk.o) \
	$(ALIB)(shsavl.o) \
	$(ALIB)(shsavm.o) \
	$(ALIB)(shsavn.o) \
	$(ALIB)(shsavp.o) \
	$(ALIB)(shsavq.o) \
	$(ALIB)(shsavs.o) \
	$(ALIB)(shsavu.o) \
	$(ALIB)(shsend.o) \
	$(ALIB)(shtscd.o) \
	$(ALIB)(shtypa.o) \
	$(ALIB)(shtypd.o) \
	$(ALIB)(shtype.o) \
	$(ALIB)(shvern.o) \
	$(ALIB)(shyear.o)
 
.PRECIOUS: $(ALIB)

all :  $(ALIB)

$(ALIB): $(LOBJS)
	$(AR) $(ARFLAGS) $@ *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<

install:

clean:
	$(RM) *.o

programs_nc:

programs_gf:

