#
#	Makefile for dcstorm 
#
include $(CONFIGDIR)/$(MAKEINC)

LIBS = \
	$(BRIDGE) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(CSYSLIB)


####################

PROG = dcstorm

PROG_OBJS =  \
	dcstorm.o \
	dcsels.o \
	dcsels_out.o \
	read_storm.o 

all : $(PROG) 

$(PROG) : $(PROG_OBJS)
	$(LINK.f) $(NOFORMAIN) $(PROG_OBJS) $(LIBS) -o $@

install : $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) sels.pack $(GEMTBL)/pack
	$(AUXINSTALL) dcstorm.hlp $(GEMHLP)/hlp

clean: 
	rm -f $(PROG_OBJS) $(PROG) *~ *.o $(GEMLINKS) 

