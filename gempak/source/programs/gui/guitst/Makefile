#########################################################################
#									#
#	Makefile for GUITST						#
#									#
#	programmmers using GUITST need to remove # in front of .o files	#
#                                                                       #
# log:									#
# T. Piper/SAIC		07/03	Removed -I$(NAWIPS_INC)			#
#########################################################################
LOCINC  = -I$(OS_INC)  
LOCLIB	= -lnxm $(OS_LIB)/xwp.a $(OS_LIB)/xw.a \
		$(OS_LIB)/ps.a $(OS_LIB)/gn.a
NOBJS	= \
	guitst.o user.o

all::	guitst

guitst:: 
	@echo Making \`$@\' in `pwd` 
	@$(MAKE) "PROG = $@" _$@ 

_guitst:: $(NOBJS)
	$(FC) $(LDFLAGS) -o $(PROG) $? \
		$(OS_LIB)/ginitp_alt.o $(OS_LIB)/gendp_alt.o \
		$(OS_LIB)/gemlib.a $(OS_LIB)/gdplot2.a \
		$(OS_LIB)/gdstream.a $(GPLT) $(DEVICE) \
		$(LOCLIB) $(CGEMLIB) $(OS_LIB)/gemlib.a \
		$(CGEMLIB) $(GPLT) $(DEVICE) $(CGEMLIB) \
		$(LOCLIB) $(XLIBS) -lm
clean::
	rm *.o
.c.o:
	$(CC) $(CFLAGS) $(LOCINC) -c $<
