#
#	Makefile for NetCDF Reanalysis decoder, "dcreanal"
#
#------------------------------------------------------------------
include $(CONFIGDIR)/$(MAKEINC)

LIBS = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(LDMLOG) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(GEMLIB) \
        $(CGEMLIB)

CFLAGS = $(COPT) -I$(NAWIPS)/unidata/ldmlog -I$(NETCDFINC)
FFLAGS = $(FOPT) -I$(GEMPAK)/source/griblib/na

PROG = dcreanal

PROG_OBJS =  \
	dcreanal.o \
	open_gemgrid.o \
	put_gemgrid.o 

all : $(PROG)
	
install : $(OS_BIN)/$(PROG)

$(OS_BIN)/$(PROG) : $(PROG)
	-@cmp -s $? $@ || \
		($(INSTALL) $? $@ && echo 'updated $@')
	-@cmp -s ncarncep1.tbl $(GEMPAKHOME)/tables/grid/ncarncep1.tbl || \
		($(AUXINSTALL) ncarncep1.tbl $(GEMPAKHOME)/tables/grid/ncarncep1.tbl && echo 'updated ncarncep1.tbl')

$(PROG) : $(PROG_OBJS)
	$(LINK.f) $(NOFORMAIN) $(PROG_OBJS) $(LIBS) $(SYSLIBS) -lm -o $@

clean:
	rm -f $(PROG_OBJS) $(PROG) *~ *.o 

reallyclean: clean
	rm -f $(PROG)


