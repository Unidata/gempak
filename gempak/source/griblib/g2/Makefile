include $(CONFIGDIR)/$(MAKEINC)

#SHELL=/bin/sh

#  If you want to enable support for PNG or JPEG2000 encoding/decoding,
#  you must specify -DUSE_PNG and/or -DUSE_JPEG2000 in the DEFS variable
#  for the C pre-processor
#  -DUSE_PNG requires libpng.a and libz.a
#  -DUSE_JPEG2000 requires libjasper.a
#

DEFS=-DUSE_JPEG2000 -DUSE_PNG

#
#   This "C" source code conatins many uses of the C++ 
#   comment style "//".  Please make sure you include the
#   appropriate compiler flag to allow the use of "//" comment indicators.
#
#CFLAGS=-q64 -O3 -qarch=auto -qcpluscmt $(INC) $(DEFS)
#CC=c99
#ARFLAGS=-X64

JAS_INC=-I$(NAWIPS)/jasper/$(NA_OS)/include
PNG_INC=-I$(GEMPAK)/source/libpng
CFLAGS = $(COPT) $(CPLUS_COMMENT) $(DEFS) $(JAS_INC) $(PNG_INC) $(JASPER_HPUX)

LIB=$(CGEMLIB)

COBJS = \
	$(LIB)(cmplxpack.o) \
	$(LIB)(drstemplates.o) \
	$(LIB)(gbits.o) \
	$(LIB)(gridtemplates.o) \
	$(LIB)(misspack.o) \
	$(LIB)(pdstemplates.o) \
	$(LIB)(g2_locfld.o) \
	$(LIB)(g2_miss.o) \
	$(LIB)(g2_pack_gp.o) \
	$(LIB)(g2_rdieee.o) \
	$(LIB)(g2_reduce.o) \
	$(LIB)(g2_unpack1.o) \
	$(LIB)(g2_unpack2.o) \
	$(LIB)(g2_unpack3.o) \
	$(LIB)(g2_unpack4.o) \
	$(LIB)(g2_unpack5.o) \
	$(LIB)(g2_unpack6.o) \
	$(LIB)(g2_unpack7.o) \
	$(LIB)(g2_free.o) \
	$(LIB)(g2_info.o) \
	$(LIB)(g2_getfld.o) \
	$(LIB)(simunpack.o) \
	$(LIB)(comunpack.o) \
	$(LIB)(specpack.o) \
	$(LIB)(specunpack.o) \
	$(LIB)(mkieee.o) \
	$(LIB)(int_power.o) \
	$(LIB)(simpack.o) \
	$(LIB)(compack.o) \
	$(LIB)(jpcpack.o) \
	$(LIB)(jpcunpack.o) \
	$(LIB)(pngpack.o) \
	$(LIB)(pngunpack.o) \
	$(LIB)(dec_jpeg2000.o) \
	$(LIB)(enc_jpeg2000.o) \
	$(LIB)(dec_png.o) \
	$(LIB)(enc_png.o) \
	$(LIB)(g2_create.o) \
	$(LIB)(g2_addlocal.o) \
	$(LIB)(g2_addgrid.o) \
	$(LIB)(g2_addfield.o) \
	$(LIB)(g2_gribend.o) \
	$(LIB)(getdim.o) \
	$(LIB)(getpoly.o) \
	$(LIB)(seekgb.o)

.PRECIOUS: $(LIB)

all: $(LIB)

$(LIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a:
	$(CC) -c $(CFLAGS) $<

clean:
	$(RM) *.o

gridtemplates.c: gridtemplates.h
pdstemplates.c: pdstemplates.h
drstemplates.c: drstemplates.h

