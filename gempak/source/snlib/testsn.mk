$(BINDIR)/testsn: -lsnlib -lgemlib -lprmcnvlib -lgemlib -lcgemlib \
		  -lappl -lsyslib -lnetcdf $(PYLIB) \
		  -ltextlib -lgemlib -lxslt -lxml2 -liconv -lz -lm
