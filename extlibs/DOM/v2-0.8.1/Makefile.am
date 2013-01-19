NULL =

SUBDIRS = 		\
	libgdome 	\
	test		\
        gtk-doc		\
	$(NULL)

confexecdir=$(libdir)
confexec_DATA = gdomeConf.sh

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA=gdome2.pc

m4dir = $(datadir)/aclocal
m4_DATA = gdome2.m4

bin_SCRIPTS = gdome-config

man_MANS = gdome-config.1

EXTRA_DIST = 			\
	COPYING.LIB 		\
	MAINTAINERS		\
	AUTHORS			\
	NEWS			\
	gdomeConf.sh.in 	\
	gdome-config.in		\
	gdome2.spec		\
	gdome2.m4		\
	gdome-config.1		\
	gdome2.pc.in		\
	README.developer	\
	$(NULL)

## We create gdomeConf.sh here and not from configure because we want
## to get the paths expanded correctly.  Macros like srcdir are given
## the value NONE in configure if the user doesn't specify them (this
## is an autoconf feature, not a bug).

gdomeConf.sh: gdomeConf.sh.in Makefile
## Use sed and then mv to avoid problems if the user interrupts.
	sed -e 's?\@GDOME_LIBDIR\@?$(GDOME_LIBDIR)?g' \
	    -e 's?\@GDOME_INCLUDEDIR\@?$(GDOME_INCLUDEDIR)?g' \
	    -e 's?\@PACKAGE\@?$(PACKAGE)?g' \
	    -e 's?\@VERSION\@?$(VERSION)?g' \
	    -e 's?\@GDOME_LIBS\@?$(GDOME_LIBS)?g' \
	       < $(srcdir)/gdomeConf.sh.in > gdomeConf.tmp \
	&& mv gdomeConf.tmp gdomeConf.sh

