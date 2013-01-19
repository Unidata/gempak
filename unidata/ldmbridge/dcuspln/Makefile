#
#	Makefile for USPLN lightning decoder, "dcuspln"
#
# Chiz                  11/99

include $(CONFIGDIR)/$(MAKEINC)

LIBS = $(BRIDGE) $(GEMLIB) $(CGEMLIB) $(GEMLIB)

PROG = dcuspln

OBJS = \
	dcuspln.o \
	bufread.o \
	decode_strike.o \
	decode_uspln.o \
	dcguspln.o \
	dmendf.o \
	dmnldn.o \
	nldninit.o \
	setcparm.o \
	sfcsdf.o \
	sfnldn.o \
	write_point.o

CFLAGS = $(COPT) -I$(GEMPAK)/source/bridge/dc
FFLAGS = $(FOPT) -I$(GEMPAK)/source/sflib -I$(GEMPAK)/source/gemlib/dm 

all : $(PROG)

$(PROG) : $(OBJS)
	$(LINK.f) $(NOFORMAIN) $(OBJS) $(LIBS) $(SYSLIBS) -o $@ 

install : $(OS_BIN)/$(PROG) installhelp
	$(AUXINSTALL) uspln.pack $(GEMTBL)/pack/uspln.pack

$(OS_BIN)/$(PROG) : $(PROG)
	-@cmp -s $? $@ || \
		($(INSTALL) $? $@ && echo 'updated $@')

installhelp: $(PROG).hlp
	-@cmp -s $? $(GEMHLP)/hlp/$? || \
		($(AUXINSTALL) $? $(GEMHLP)/hlp/$? && echo 'updated $?')

clean: 
	rm -f $(OBJS) $(PROG) 20061013*_uspln.gem

test: $(PROG)
	cat 2006101320.ingest | $(PROG) -v 4 -d - -m 1000 -b 20 -s minute05 -p uspln.pack YYYYMMDDHHNN_uspln.gem
