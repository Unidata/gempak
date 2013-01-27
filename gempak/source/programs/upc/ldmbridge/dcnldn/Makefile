#
#	Makefile for NLDN lightning decoder, "dcnldn"
#
# Chiz                  11/99

include $(CONFIGDIR)/$(MAKEINC)

LIBS = $(BRIDGE) $(GEMLIB) $(CGEMLIB) $(GEMLIB)

PROG = dcnldn

OBJS = \
	dcnldn.o \
	dcgnldn.o \
	decode_nldn.o \
	dmendf.o \
	dmnldn.o \
	nldninit.o \
	setcparm.o \
	sfcsdf.o \
	sfnldn.o \
	write_nldn.o

CFLAGS = $(COPT) -I$(GEMPAK)/source/bridge/dc
FFLAGS = $(FOPT) -I$(GEMPAK)/source/sflib/ -I$(GEMPAK)/source/gemlib/dm 

all : $(PROG)

$(PROG) : $(OBJS)
	$(LINK.f) $(NOFORMAIN) $(OBJS) $(LIBS) $(SYSLIBS) -o $@ 

install : $(OS_BIN)/$(PROG) installhelp
	$(AUXINSTALL) nldn.pack.basic $(GEMTBL)/pack
	$(AUXINSTALL) nldn.pack.full $(GEMTBL)/pack
	$(AUXINSTALL) nldn.pack.full $(GEMTBL)/pack/nldn.pack

$(OS_BIN)/$(PROG) : $(PROG)
	-@cmp -s $? $@ || \
		($(INSTALL) $? $@ && echo 'updated $@')

$(MANDIR)/man1/$(PROG).1 : $(PROG).1
	-@cmp -s $? $@ || \
		($(AUXINSTALL) $? $@ && echo 'updated $@')

installhelp: dcnldn.hlp
	-@cmp -s $? $(GEMHLP)/hlp/$? || \
		($(AUXINSTALL) $? $(GEMHLP)/hlp/$? && echo 'updated $?')

clean: 
	rm -f $(OBJS) $(PROG) 99113016??_nldn.gem*

test:
	cat 9933416.ingest | dcnldn -v 1 -d - -m 1000 -b 20 -s minute05 -p nldn.pack YYMMDDHHNN_nldn.gem
