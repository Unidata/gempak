$(BINDIR)/testgd: -lgridlib -lgemlib -lappl $(PYLIB) \
		  -ltextlib -lgridlib -lxslt -lxml2 -liconv \
		  -lsyslib -lnetcdf -lz -lm
