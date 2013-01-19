#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 9/08 M.James
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(NXMLIB)

COBJS =	\
	$(ALIB)(spfclnup.o) \
	$(ALIB)(spfclose.o) \
	$(ALIB)(spfcreate.o) \
	$(ALIB)(spfgtfld.o) \
	$(ALIB)(spfinit.o) \
	$(ALIB)(spfload.o) \
	$(ALIB)(spfopen.o) \
	$(ALIB)(spfread.o) \
	$(ALIB)(spfwrite.o) \
	$(ALIB)(NuiColorInit.o) \
	$(ALIB)(NxmBusy.o) \
	$(ALIB)(NxmBxmBtn.o) \
	$(ALIB)(NxmClose.o) \
	$(ALIB)(NxmClrW.o) \
	$(ALIB)(NxmColorBar.o) \
	$(ALIB)(NxmColorBlink.o) \
	$(ALIB)(NxmColorEdit.o) \
	$(ALIB)(NxmColorNameLists.o) \
	$(ALIB)(NxmColorPalette.o) \
	$(ALIB)(NxmColorSliders.o) \
	$(ALIB)(NxmColorTable.o) \
	$(ALIB)(NxmColrP.o) \
	$(ALIB)(NxmConfirm.o) \
	$(ALIB)(NxmCtlBtn.o) \
	$(ALIB)(NxmCursor.o) \
	$(ALIB)(NxmDwell.o) \
	$(ALIB)(NxmEnhw.o) \
	$(ALIB)(NxmErr.o) \
	$(ALIB)(NxmExit.o) \
	$(ALIB)(NxmGeneric.o) \
	$(ALIB)(NxmGif.o) \
	$(ALIB)(NxmGmpk.o) \
	$(ALIB)(NxmHelp.o) \
	$(ALIB)(NxmInitialize.o) \
	$(ALIB)(NxmLabel.o) \
	$(ALIB)(NxmLineA.o) \
	$(ALIB)(NxmLoopPanel.o) \
	$(ALIB)(NxmMarkA.o) \
	$(ALIB)(NxmMenuPulldown.o) \
	$(ALIB)(NxmPrompt.o) \
	$(ALIB)(NxmPrt.o) \
	$(ALIB)(NxmPushBtnMenu.o) \
	$(ALIB)(NxmResCheck.o) \
	$(ALIB)(NxmResources.o) \
	$(ALIB)(NxmScaleA.o) \
	$(ALIB)(NxmTxtIn.o) \
	$(ALIB)(NxmVersShowTitle.o) \
	$(ALIB)(NxmVolcano.o) \
	$(ALIB)(NxmWarn.o)


GDIR = spf

.PRECIOUS: $(ALIB)

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC)

all : $(ALIB)

static : all

$(ALIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

install:

