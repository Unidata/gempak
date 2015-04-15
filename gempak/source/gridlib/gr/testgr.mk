$(BINDIR)/testgr: -lgridlib -lgemlib -lappl -lgemlib \
		  -ltextlib -lxslt -lxml2 -liconv $(PYLIB) \
		  -lsyslib -lnetcdf -lz -lm
