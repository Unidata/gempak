NULL =

SUBDIRS = 		\
	gdomecore 	\
        events		\
        xpath		\
	$(NULL)

INCLUDES = 					\
	-I$(top_srcdir)/libgdome 		\
	-I$(top_srcdir)/libgdome/gdomecore 	\
	-I$(top_srcdir)/libgdome/events 	\
	-I$(top_srcdir)/libgdome/xpath	 	\
	 $(LIBXML_CFLAGS) $(GLIB_CFLAGS) 	\
	$(NULL)

lib_LTLIBRARIES = libgdome.la

libgdome_la_SOURCES = 			\
	gdome.c				\
        gdome-events.c			\
        gdome-xpath.c			\
        gdome-util.c			\
        gdome-private-list.c		\
        gdome-refdebug.c		\
        gdome-treegc.c			\
	$(NULL)


libgdome_la_LIBADD = 						\
	$(top_builddir)/libgdome/gdomecore/libgdomecore.la	\
	$(top_builddir)/libgdome/events/libgdomeevents.la	\
	$(top_builddir)/libgdome/xpath/libgdomexpath.la		\
        $(LIBXML_LIBS)						\
        $(GLIB_LIBS)                                            \
	$(NULL)
        
libgdome_la_LDFLAGS = -version-info @GDOME_VERSION_INFO@

# what is this include dir ?
libgdomeincdir = $(includedir)/libgdome

libgdomeinc_HEADERS = 			\
	gdome.h				\
        gdome-events.h			\
        gdome-xpath.h			\
        gdome-util.h			\
        gdome-private-list.h		\
        gdome-refdebug.h		\
        gdome-treegc.h			\
        gdome-libxml-util.h		\
	$(NULL)
