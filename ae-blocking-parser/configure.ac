dnl **************************************************************
dnl AESOP AUTOCONF SCRIPT
dnl
dnl Process this file with autoconf to produce a configure script.
dnl You may need to use autoheader as well if changing any DEFINEs

CONFIGURE_TIME=`date -u +"%Y-%m-%d-%H%M%S"`
AESOP_MAJOR=0
AESOP_MINOR=1
AESOP_SUB=1
AESOP_PRE="pre-$CONFIGURE_TIME"

AESOP_VERSION=$AESOP_MAJOR.$AESOP_MINOR.$AESOP_SUB$AESOP_PRE

AC_PREREQ(2.50)
AC_INIT([ae-blocking-parser], [0.1.2], [http://trac.mcs.anl.gov/projects/aesop],[],[http://www.mcs.anl.gov/projects/aesop])

AC_CONFIG_AUX_DIR(maint/config)

AM_INIT_AUTOMAKE([foreign subdir-objects -Wall])

AM_SILENT_RULES([yes])

AC_SUBST(AESOP_VERSION)

AC_DEFINE_UNQUOTED(AESOP_VERSION_MAJOR, $AESOP_MAJOR, major version number)
AC_DEFINE_UNQUOTED(AESOP_VERSION_MINOR, $AESOP_MINOR, minor version number)
AC_DEFINE_UNQUOTED(AESOP_VERSION_SUB,   $AESOP_SUB, sub version number)

AC_CONFIG_HEADER(ae-blocking-parser-config.h)

AC_PROG_INSTALL
AC_PROG_CC

AX_GHC("")

GHCFLAGS="-XFlexibleInstances"

dnl enable static builds of parser binary
AC_ARG_ENABLE(static-parser,
    [AS_HELP_STRING(
        [--enable-static-parser],
        [build static ae-blocking-parser with no GHC runtime dependencies])
    ])
AS_IF([test "x$enable_static_parser" = "xyes"], [
    GHCFLAGS="$GHCFLAGS -static -optc-static -optl-static -fvia-C -optl-pthread"
])

AC_ARG_ENABLE([bindist],
[  --enable-bindist    Include ae-blocking-parser binary in make dist],
[case "${enableval}" in
  yes) bindist=true ;;
  no)  bindist=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-bindist]) ;;
esac],[bindist=false])
AM_CONDITIONAL([BINDIST], [test x$bindist = xtrue])

AC_SUBST(GHCFLAGS)

AC_CONFIG_FILES([Makefile])
AC_CONFIG_COMMANDS([mkdir], [$MKDIR_P src])

AC_OUTPUT

