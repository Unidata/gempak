#
#	Makefile for GEMPAK 6.0
#		Builds gplt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GPLT)

FOBJS = \
	$(ALIB)(gqarrw.o) \
	$(ALIB)(gqbarb.o) \
	$(ALIB)(gqcmbo.o) \
	$(ALIB)(gqctyp.o) \
	$(ALIB)(gqcvsc.o) \
	$(ALIB)(gqdarr.o) \
	$(ALIB)(gqdash.o) \
	$(ALIB)(gqdatt.o) \
	$(ALIB)(gqfill.o) \
	$(ALIB)(gqfrnt.o) \
	$(ALIB)(gqhash.o) \
	$(ALIB)(gqicng.o) \
	$(ALIB)(gqline.o) \
	$(ALIB)(gqlpat.o) \
	$(ALIB)(gqmrkr.o) \
	$(ALIB)(gqptnd.o) \
	$(ALIB)(gqpwth.o) \
	$(ALIB)(gqrduc.o) \
	$(ALIB)(gqsizd.o) \
	$(ALIB)(gqsky.o) \
	$(ALIB)(gqsmth.o) \
	$(ALIB)(gqspcl.o) \
	$(ALIB)(gqspln.o) \
	$(ALIB)(gqsysz.o) \
	$(ALIB)(gqtext.o) \
	$(ALIB)(gqturb.o) \
	$(ALIB)(gqwthr.o) \
	$(ALIB)(gsarrw.o) \
	$(ALIB)(gsattr.o) \
	$(ALIB)(gsbarb.o) \
	$(ALIB)(gscmbo.o) \
	$(ALIB)(gscntr.o) \
	$(ALIB)(gsctyp.o) \
	$(ALIB)(gsdarr.o) \
	$(ALIB)(gsdash.o) \
	$(ALIB)(gsfill.o) \
	$(ALIB)(gsfrnt.o) \
	$(ALIB)(gshash.o) \
	$(ALIB)(gsicng.o) \
	$(ALIB)(gsline.o) \
	$(ALIB)(gslpat.o) \
	$(ALIB)(gsmrkr.o) \
	$(ALIB)(gsptnd.o) \
	$(ALIB)(gspwth.o) \
	$(ALIB)(gsrduc.o) \
	$(ALIB)(gssky.o) \
	$(ALIB)(gssmth.o) \
	$(ALIB)(gsspcl.o) \
	$(ALIB)(gsspln.o) \
	$(ALIB)(gstext.o) \
	$(ALIB)(gstick.o) \
	$(ALIB)(gsturb.o) \
	$(ALIB)(gswthr.o)

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

