NULL =

SUBDIRS = 		\
	apigen	       	\
	$(NULL)

INCLUDES =					\
	-DLOCALDIR=\""$(srcdir)"\"		\
	-I$(srcdir) 				\
	-I$(top_srcdir)/libgdome 		\
	-I$(top_srcdir)/libgdome/gdomecore 	\
	$(LIBXML_CFLAGS) $(GLIB_CFLAGS)        	\
	$(NULL)

noinst_PROGRAMS =		\
	test-document		\
        test-node		\
        test-cdata		\
        test-dtd		\
        test-element		\
        test-nodelist		\
        test-namednodemap	\
        examplea		\
        exampleb		\
        examplec		\
        test-mevents		\
        test-loadsave		\
        test-str		\
        test-treegc		\
        test-xpath		\
	test-importnode		\
        bench			\
        $(NULL)

DEPS = $(top_builddir)/libgdome/libgdome.la
LDADDS = $(top_builddir)/libgdome/libgdome.la $(LIBXML_LIBS) $(GLIB_LIBS)

test_document_SOURCES = test-document.c
test_document_LDFLAGS =
test_document_DEPENDENCIES = $(DEPS)
test_document_LDADD = $(LDADDS)

test_node_SOURCES = test-node.c
test_node_LDFLAGS =
test_node_DEPENDENCIES = $(DEPS)
test_node_LDADD = $(LDADDS)

test_cdata_SOURCES = test-cdata.c
test_cdata_LDFLAGS =
test_cdata_DEPENDENCIES = $(DEPS)
test_cdata_LDADD = $(LDADDS)

test_dtd_SOURCES = test-dtd.c
test_dtd_LDFLAGS =
test_dtd_DEPENDENCIES = $(DEPS)
test_dtd_LDADD = $(LDADDS)

test_element_SOURCES = test-element.c
test_element_LDFLAGS =
test_element_DEPENDENCIES = $(DEPS)
test_element_LDADD = $(LDADDS)

test_nodelist_SOURCES = test-nodelist.c
test_nodelist_LDFLAGS =
test_nodelist_DEPENDENCIES = $(DEPS)
test_nodelist_LDADD = $(LDADDS)

test_namednodemap_SOURCES = test-namednodemap.c
test_namednodemap_LDFLAGS =
test_namednodemap_DEPENDENCIES = $(DEPS)
test_namednodemap_LDADD = $(LDADDS)

examplea_SOURCES = examplea.c
examplea_LDFLAGS =
examplea_DEPENDENCIES = $(DEPS)
examplea_LDADD = $(LDADDS)

exampleb_SOURCES = exampleb.c
exampleb_LDFLAGS =
exampleb_DEPENDENCIES = $(DEPS)
exampleb_LDADD = $(LDADDS)

examplec_SOURCES = examplec.c
examplec_LDFLAGS =
examplec_DEPENDENCIES = $(DEPS)
examplec_LDADD = $(LDADDS)

test_mevents_SOURCES = test-mevents.c
test_mevents_LDFLAGS =
test_mevents_DEPENDENCIES = $(DEPS)
test_mevents_LDADD = $(LDADDS)

test_loadsave_SOURCES = test-loadsave.c
test_loadsave_LDFLAGS =
test_loadsave_DEPENDENCIES = $(DEPS)
test_loadsave_LDADD = $(LDADDS)

test_str_SOURCES = test-str.c
test_str_LDFLAGS =
test_str_DEPENDENCIES = $(DEPS)
test_str_LDADD = $(LDADDS)

test_treegc_SOURCES = test-treegc.c
test_treegc_LDFLAGS =
test_treegc_DEPENDENCIES = $(DEPS)
test_treegc_LDADD = $(LDADDS)

test_xpath_SOURCES = test-xpath.c
test_xpath_LDFLAGS =
test_xpath_DEPENDENCIES = $(DEPS)
test_xpath_LDADD = $(LDADDS)

test_importnode_SOURCES = test-importnode.c
test_importnode_LDFLAGS =
test_importnode_DEPENDENCIES = $(DEPS)
test_importnode_LDADD = $(LDADDS)

bench_SOURCES = bench.c
bench_LDFLAGS =
bench_DEPENDENCIES = $(DEPS)
bench_LDADD = $(LDADDS)

EXTRA_DIST =				\
	exampleb.xml			\
	examplec.xml			\
	test-document2.xml       	\
	test-document3.xml     		\
	test-dtd.xml			\
        test-dtd.dtd			\
	test-element.xml		\
	test-node.xml			\
	test-nodelist.xml		\
        test-namednodemap.xml		\
        test-mevents.xml		\
        test-loadsave1.xml		\
        test-loadsave2.xml		\
	test.xml			\
        test-treegc.xml			\
        test-xpath.xml			\
	$(NULL)

TESTS_ENVIROMENT = 					\
	LD_LIBRARY_PATH=$(top_builddir)/libgdome/	\
        $(NULL)
        
TESTS =		 		\
	test-document		\
        test-node		\
        test-cdata		\
        test-dtd		\
        test-element		\
        test-nodelist		\
        test-namednodemap	\
        examplea		\
        exampleb		\
        examplec		\
        test-mevents		\
        test-loadsave		\
        test-str		\
        test-treegc		\
        test-xpath		\
	test-importnode		\
        $(NULL)
