#
#	Makefile for GRIB decoder, "dcgrib"
#
# P.Bruehl	5/95	Updated for gribtogem version 2
# CHIZ         12/95    rewritten for decoders-2.1 and gempak5.2.1
#------------------------------------------------------------------
include $(CONFIGDIR)/$(MAKEINC)

LIBS = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
	$(ZLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(LDMLOG)

#######

PROG = dcgrib

PROG_OBJS =  \
	dcgrib.o \
	bd_gem_grid.o \
	open_gem_grid.o \
	put_gem_grid.o \
	gempds.o \
	gemgds.o \
	stagger.o \
	stagger_e.o \
	emalloc.o \
	gbds.o \
	gbytem.o \
	gdes.o \
	get_prod.o \
	grib1.o \
	gribtypes.o \
	params.o \
	product_data.o \
	quasi.o 
#	inetutil.o 

SYMLINKS = \
	centers.h \
	emalloc.c \
	emalloc.h \
	gbds.h \
	gbds.c \
	gbytem.c \
	gbytem.h \
	gdes.h \
	gdes.c \
	gds.h \
	get_prod.c \
	get_prod.h \
	grib1.c \
	grib1.h \
	gribtypes.c \
	gribtypes.h \
	params.c \
	params.h \
	product_data.c \
	product_data.h \
	quasi.c \
	quasi.h 

##	inetutil.h 
#	inetutil.c 

DECODEDIR = ../gribtonc_src

CFLAGS = $(COPT) -DGEMPAK -I$(GEMPAK)/source/programs/upc/ldmlog

all : $(SYMLINKS) nacmn.cmn gbcmn.h $(PROG)
	
install : $(OS_BIN)/$(PROG) $(GEMHLP)/hlp/$(PROG).hlp

$(OS_BIN)/$(PROG) : $(PROG)
	-@cmp -s $? $@ || \
		($(INSTALL) $? $@ && echo 'updated $@')

$(GEMHLP)/hlp/$(PROG).hlp : $(PROG).hlp
	-@cmp -s $? $@ || \
		($(AUXINSTALL) $? $@ && echo 'updated $@')

$(MANDIR)/man1/$(PROG).1 : $(PROG).1
	-@cmp -s $? $@ || \
		($(AUXINSTALL) $? $@ && echo 'updated $@')

$(SYMLINKS) : 
	ln -s $(DECODEDIR)/$@ $@

nacmn.cmn: 
	-$(LN) $(GEMPAK)/source/griblib/na/nacmn.cmn .

gbcmn.h:
	-$(LN) $(GEMPAK)/source/griblib/gb/gbcmn.h .

$(PROG) : $(SYMLINKS) nacmn.cmn gbcmn.h $(PROG_OBJS)
	$(LINK.f) $(NOFORMAIN) -o $@ \
		$(PROG_OBJS) $(LIBS) $(SYSLIBS)

clean:
	rm -f $(PROG_OBJS) $(PROG) *~ $(SYMLINKS) *.o ulog.h nacmn.cmn gbcmn.h

reallyclean: clean
	rm -f $(PROG)


