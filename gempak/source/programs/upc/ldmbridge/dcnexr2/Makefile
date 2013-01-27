include $(CONFIGDIR)/$(MAKEINC)

COBJS = \
	dcnexr2.o \
	bufread.o

CFLAGS=$(COPT) -I$(GEMPAK)/source/bzip2 -I$(GEMPAK)/source/bridge/dc

LIBINC = \
	$(BRIDGE) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

all: dcnexr2

dcnexr2: $(COBJS)
	$(LINK.f) $(NOFORMAIN) -o $@ $(COBJS) $(LIBINC) $(LIBBZ2) -lm

install: dcnexr2
	$(AUXINSTALL) dcnexr2 $(OS_BIN)
	$(AUXINSTALL) $?.hlp $(GEMHLP)/hlp


clean:
	$(RM) *.o core dcnexr2
