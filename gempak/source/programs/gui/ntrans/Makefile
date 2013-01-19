#
#	Makefile for nmap
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROG = ntrans

ALIB = $(OS_LIB)/ntrans.a

OBJS = \
	$(ALIB)(autogroup.o) \
	$(ALIB)(byte_flip.o) \
	$(ALIB)(commands.o) \
	$(ALIB)(drawingw.o) \
	$(ALIB)(dsply.o) \
	$(ALIB)(file.o) \
	$(ALIB)(gp_callbks.o) \
	$(ALIB)(graf_init.o) \
	$(ALIB)(grouplist.o) \
	$(ALIB)(legend.o) \
	$(ALIB)(local.o) \
	$(ALIB)(mainw.o) \
	$(ALIB)(man_group.o) \
	$(ALIB)(menubar.o) \
	$(ALIB)(models.o) \
	$(ALIB)(multipanel.o) \
	$(ALIB)(ntrans_spool.o) \
	$(ALIB)(npfcreate.o) \
	$(ALIB)(npfload.o) \
	$(ALIB)(npfopen.o) \
	$(ALIB)(npfw.o) \
	$(ALIB)(process_meta.o) \
	$(ALIB)(select_frame.o) \
	$(ALIB)(select_group.o)

LIBINC = \
	$(GPLT_NC) \
	$(NXMLIB) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
        $(OS_LIB)/xwp.a \
        $(OS_LIB)/xw.a \
        $(PS) \
        $(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB) \
	$(GPLT)

.PRECIOUS: $(ALIB) 

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC)

all : $(ALIB) $(PROG)

$(PROG) : $(LIBINC)
	$(COMPILE.c) $(PROG).c
	$(LINK.f) -o $@ \
		$(PROG).o \
		$(NOFORMAIN) \
		$(ALIB) \
		$(LIBINC) \
		$(GUILIBS) $(SYSLIBS) -lm
	$(RM) *.o

$(ALIB): $(OBJS)
	$(AR) $(ARFLAGS) $@ $?
	$(RM) $?

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) $(PROG) *.o

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
