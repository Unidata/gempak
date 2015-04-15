$(BINDIR)/testpc: -lsflib -lsnlib -lgemlib -lprmcnvlib \
		  -lgemlib -ltextlib -lxslt -lxml2 -liconv $(PYLIB) \
		  -lappl -lsyslib -lnetcdf -lz -lm
