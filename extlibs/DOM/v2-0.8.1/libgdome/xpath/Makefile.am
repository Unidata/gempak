NULL = 
INCLUDES = \
	-I$(top_srcdir)/libgdome 		\
	-I$(top_srcdir)/libgdome/gdomecore 	\
	 $(LIBXML_CFLAGS) 			\
	$(GLIB_CFLAGS) 				\
	$(NULL)

noinst_LTLIBRARIES = libgdomexpath.la

libgdomexpath_la_SOURCES = 		\
	gdome-xpath-xpeval.c		\
	gdome-xpath-xpnsresolv.c	\
        gdome-xpath-xpresult.c		\
	$(NULL)		

EXTRA_DIST =				\
	gdome-xpath-xpeval.h		\
	gdome-xpath-xpnsresolv.h	\
        gdome-xpath-xpresult.h		\
	$(NULL)		

# libgdomeincdir = $(includedir)/libgdome
