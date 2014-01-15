#
include $(CONFIGDIR)/$(MAKEINC)

LIBS = $(BRIDGE) $(CGEMLIB) $(GEMLIB) $(LDMLOG) $(XMLLIB) $(ZLIB)

####################

PROG = dctrop

PROG_OBJS =  \
        dctrop.o \
        dctropic.o \
        read_nhc.o \
        read_guam.o

CFLAGS = $(COPT) -I$(GEMPAK)/source/programs/upc/ldmlog

all : $(PROG) 

$(PROG) : $(PROG_OBJS)
	$(LINK.f) $(NOFORMAIN) $(PROG_OBJS) $(LIBS) -o $@

install : $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) tropic.pack $(GEMTBL)/pack
	$(AUXINSTALL) dctrop.hlp $(GEMHLP)/hlp

test: $(PROG)
	cat test1 | dctrop -v 4 -d - @@.gem

clean: 
	rm -f $(PROG_OBJS) $(PROG) *~ *.o blanca.gem

