# GDOME2_DEFS([GDOME2_REQS=gdome2])
# ---------------------------------------
AC_DEFUN([GDOME2_DEFS],
[
       dnl Import gdome2 data
       PKG_CHECK_MODULES(GDOME2,m4_default([$1], gdome2))
       AC_SUBST(GDOME2_CFLAGS)
       AC_SUBST(GDOME2_LIBS)

       dnl Example for exporting other variables read from pkg-config
       dnl foodir=`$PKG_CONFIG --variable=foodir "gdome2"`
       dnl AC_SUBST(foodir)
])
