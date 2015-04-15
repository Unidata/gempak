$(BINDIR)/testsf: -lsflib -lprmcnvlib -lgemlib -lappl \
		  -lsyslib -lnetcdf $(PYLIB) \
		  -ltextlib -lxslt -lxml2 -liconv -lz -lm
