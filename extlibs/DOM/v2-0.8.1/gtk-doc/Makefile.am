## Process this file with automake to produce Makefile.in

# The name of the module.
DOC_MODULE=gdome2-$(VERSION)

# The top-level SGML file.
DOC_MAIN_SGML_FILE=gdome2-docs.sgml

# The directory containing the source code (if it contains documentation).
DOC_SOURCE_DIR=../libgdome

HTML_DIR=@HTML_DIR@

TARGET_DIR=$(HTML_DIR)/$(DOC_MODULE)

scan:
	gtkdoc-scan --module=gdome2 $(DOC_SOURCE_DIR)/gdome.h $(DOC_SOURCE_DIR)/gdome-private-list.h $(DOC_SOURCE_DIR)/gdome-events.h $(DOC_SOURCE_DIR)/gdome-xpath.h $(DOC_SOURCE_DIR)/gdome-treegc.h

templates: scan
	gtkdoc-mktmpl --module=gdome2

sgml:
	gtkdoc-mkdb --module=gdome2 --source-dir=$(DOC_SOURCE_DIR)

html:
	if ! test -d html ; then mkdir html ; fi
	-cd html && gtkdoc-mkhtml gdome2 ../$(DOC_MAIN_SGML_FILE)

clean-local:
	rm -f *~ *.bak *.hierarchy *.signals *.args *-unused.txt

maintainer-clean-local: clean
	rm -rf sgml html gdome2-decl-list.txt gdome2-decl.txt 

install-data-local:
	-install -d -m 0755 $(TARGET_DIR)
	-install -m 0644 $(srcdir)/html/*.html $(TARGET_DIR)
	-install -m 0644 $(srcdir)/html/index.sgml $(TARGET_DIR)
	-(cd $(DESTDIR); gtkdoc-fixxref --module=$(DOC_MODULE) --html-dir=$(HTML_DIR))

.PHONY : html sgml templates scan

NULL =

EXTRA_DIST =							\
	gdome2-docs.sgml					\
        gdome2-sections.txt					\
        html/book1.html						\
        html/gdome2-basic-types-and-enumerations.html		\
	html/gdome2-core.html					\
	html/gdome2-gdomeattr.html				\
	html/gdome2-gdomecdatasection.html			\
	html/gdome2-gdomecharacterdata.html			\
	html/gdome2-gdomecomment.html				\
	html/gdome2-gdomedocument.html				\
	html/gdome2-gdomedocumentfragment.html			\
	html/gdome2-gdomedocumenttype.html			\
	html/gdome2-gdomedomimplementation.html			\
	html/gdome2-gdomeelement.html				\
	html/gdome2-gdomeentity.html				\
	html/gdome2-gdomeentityreference.html			\
	html/gdome2-gdomenamednodemap.html			\
	html/gdome2-gdomenode.html				\
	html/gdome2-gdomenodelist.html				\
	html/gdome2-gdomenotation.html				\
	html/gdome2-gdomeprocessinginstruction.html		\
	html/gdome2-gdometext.html				\
	html/gdome2-events-basic-types-and-enumerations.html	\
	html/gdome2-events.html					\
	html/gdome2-gdomeevent.html				\
	html/gdome2-gdomeeventlistener.html			\
        html/gdome2-xpath.html					\
        html/gdome2-xpath-basic-types-and-enumerations.html	\
	html/gdome2-gdomemutationevent.html			\
	html/gdome2-gdomexpathevaluator.html   			\
	html/gdome2-gdomexpathnamespace.html   			\
	html/gdome2-gdomexpathnsresolver.html			\
	html/gdome2-gdomexpathresult.html      			\
        html/gdome2-memory.html					\
        html/gdome2-module-private-list.html			\
        html/gdome2-module-tree-garbage-collector.html 		\
	html/index.sgml						\
        $(NULL)
