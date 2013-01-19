AUTOMAKE_OPTIONS = foreign

docdir = $(prefix)/doc/$(PACKAGE)
doc_DATA = *.html *.gif *.jpg

EXTRA_DIST = $(doc_DATA) *.fig 

install-exec-hook:
	-mkdir $(prefix)/doc
	-mkdir $(docdir)
	$(INSTALL) -m 444 ../README $(docdir)
	$(INSTALL) -m 444 ../CHANGES $(docdir)
