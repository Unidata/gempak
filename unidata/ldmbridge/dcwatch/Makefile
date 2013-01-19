#
#	Makefile for Watch Box decoder, "dcwatch"
#
include $(CONFIGDIR)/$(MAKEINC)

LIBS = \
	$(GEMLIB) \
	$(BRIDGE) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(LDMLOG) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(GEMLIB) \
        $(CGEMLIB)

####################

PROG = dcwatch

PROG_OBJS =  \
	dcwatch.o \
	dcwwdc.o \
	dcwwus40.o \
	dcwwstore.o \
	wwus40_tools.o

CFLAGS = $(COPT) -I$(NAWIPS)/unidata/ldmlog

all : $(PROG) 

$(PROG) : $(PROG_OBJS)
	$(LINK.f) $(NOFORMAIN) $(PROG_OBJS) $(LIBS) $(SYSLIBS) -o $@

install : $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) watch.pack $(GEMTBL)/pack
	$(AUXINSTALL) dcwatch.hlp $(GEMHLP)/hlp

clean:
	rm -f $(PROG_OBJS) $(PROG) *~ *.o 

reallyclean: clean
	rm -f $(PROG)
