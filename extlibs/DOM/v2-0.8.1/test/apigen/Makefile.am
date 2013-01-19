NULL =

INCLUDES =					\
	-I$(top_srcdir)/libgdome 		\
	$(LIBXML_CFLAGS) $(GLIB_CFLAGS)        	\
	$(NULL)

noinst_PROGRAMS =	\
	apigen		\
        $(NULL)

DEPS = $(top_builddir)/libgdome/libgdome.la
LDADDS = $(top_builddir)/libgdome/libgdome.la $(LIBXML_LIBS) $(GLIB_LIBS)

apigen_SOURCES = 	\
	util.c		\
        srcutil.c	\
        apigen.c	\
        $(NULL)

apigen_LDFLAGS =
apigen_DEPENDENCIES = $(DEPS)
apigen_LDADD = $(LDADDS)

EXTRA_DIST =	      	\
	util.h		\
        srcutil.h	\
        core.xml	\
        events.xml	\
        traversal.xml	\
        xpath.xml	\
	$(NULL)

