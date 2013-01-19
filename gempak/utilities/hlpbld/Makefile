include $(CONFIGDIR)/$(MAKEINC)


#[if  [ $OS = "HPUX" ]
#then
#    MYFLAG="+U77"
#else
#    MYFLAG=""
#fi

all: hlpbld

hlpbld: hlpbld.f
	$(LINK.f) -o hlpbld hlpbld.f $(GEMLIB) $(CGEMLIB) $(GEMLIB)

clean:
	$(RM) *.o hlpbld

install: hlpbld
	$(INSTALL) hlpbld $(OS_BIN)
