NULL = 
INCLUDES = \
	-I$(top_srcdir)/libgdome 		\
	-I$(top_srcdir)/libgdome/gdomecore 	\
	 $(LIBXML_CFLAGS) 			\
	$(GLIB_CFLAGS) 				\
	$(NULL)

noinst_LTLIBRARIES = libgdomeevents.la

libgdomeevents_la_SOURCES = 		\
	gdome-evt-mevent.c		\
	gdome-evt-eventl.c		\
	gdome-evt-event.c		\
        gdome-evt-propagation.c		\
	$(NULL)		

EXTRA_DIST =				\
	gdome-evt-mevent.h		\
	gdome-evt-eventl.h		\
	gdome-evt-event.h		\
        gdome-evt-propagation.h		\
        gdome-evt-util.h		\
	$(NULL)		

# libgdomeincdir = $(includedir)/libgdome
