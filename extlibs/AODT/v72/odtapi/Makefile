#
#	Makefile for GEMPAK 6.0
#		Builds libaodt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/libAODT_v72.a

COBJS = \
	$(ALIB)(odtatcfoutputfile.o) \
	$(ALIB)(odtautomode1.o) \
	$(ALIB)(odtautomode2.o) \
	$(ALIB)(odtbulletinoutput.o) \
	$(ALIB)(odtdiagnostics.o) \
	$(ALIB)(odteyecloudtemp.o) \
	$(ALIB)(odtfreememory.o) \
	$(ALIB)(odtgetwindprofile.o) \
	$(ALIB)(odtgsIRimage.o) \
	$(ALIB)(odtgsdomain.o) \
	$(ALIB)(odtgsforecastfile.o) \
	$(ALIB)(odtgshistory.o) \
	$(ALIB)(odtgslocation.o) \
	$(ALIB)(odtgsmiscopts.o) \
	$(ALIB)(odtgsscenetypes.o) \
	$(ALIB)(odtgssstvalue.o) \
	$(ALIB)(odtgsstartstr.o) \
	$(ALIB)(odtgstopovalue.o) \
	$(ALIB)(odtgsversion.o) \
	$(ALIB)(odthistorybullfmt.o) \
	$(ALIB)(odthistorycomment.o) \
	$(ALIB)(odthistorydelete.o) \
	$(ALIB)(odthistoryinsertrec.o) \
	$(ALIB)(odthistorylistfmt.o) \
	$(ALIB)(odthistorynextrec.o) \
	$(ALIB)(odthistorywrite.o) \
	$(ALIB)(odtinitialize.o) \
	$(ALIB)(odtintensityest.o) \
	$(ALIB)(odtloadIRimage.o) \
	$(ALIB)(odtmessages.o) \
	$(ALIB)(odtreadtopofile.o) \
	$(ALIB)(odtscenemap.o) \
	$(ALIB)(odtscenetype.o) \
	$(ALIB)(odtsetdatetime.o) \
	$(ALIB)(odtwarmeyetemp.o)
 
.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<

install:

clean:
	$(RM) *.o 

