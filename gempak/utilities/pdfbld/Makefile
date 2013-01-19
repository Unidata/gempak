include $(CONFIGDIR)/$(MAKEINC)


#[if  [ $OS = "HPUX" ]
#then
#    MYFLAG="+U77"
#else
#    MYFLAG=""
#fi

all: pdfbld

pdfbld: pdfbld.f
	$(LINK.f) -o pdfbld pdfbld.f $(GEMLIB) $(CGEMLIB) $(GEMLIB)

clean:
	$(RM) *.o pdfbld

install: pdfbld
	$(INSTALL) pdfbld $(OS_BIN)
