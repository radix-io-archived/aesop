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
AC_INIT([aesop], [0.2.1], [http://trac.mcs.anl.gov/projects/aesop],[],[http://www.mcs.anl.gov/projects/aesop])

AC_SUBST(AESOP_VERSION)

AC_DEFINE_UNQUOTED(AESOP_VERSION_MAJOR, $AESOP_MAJOR, major version number)
AC_DEFINE_UNQUOTED(AESOP_VERSION_MINOR, $AESOP_MINOR, minor version number)
AC_DEFINE_UNQUOTED(AESOP_VERSION_SUB,   $AESOP_SUB, sub version number)

AC_CONFIG_HEADER(aesop-config.h)
AC_CONFIG_AUX_DIR(maint/config)

AM_INIT_AUTOMAKE([foreign subdir-objects -Wall])
AM_SILENT_RULES([yes])

AC_PROG_INSTALL
AC_PROG_CC
AC_PROG_CPP
AC_PROG_RANLIB

AC_MSG_CHECKING([for required gcc])
if test "x$GCC" = "x"; then
	AC_MSG_ERROR([no])
fi
AC_MSG_RESULT(yes)

dnl
dnl Verify pkg-config
dnl
PKG_PROG_PKG_CONFIG

AC_ARG_VAR(AE_BLOCKING_PARSER, ae-blocking-parser command)

dnl Check for the aesop parser
AC_MSG_CHECKING(for aesop parser)
if test -n "$AE_BLOCKING_PARSER" ; then
    if test ! -x "$AE_BLOCKING_PARSER" ; then
        AC_MSG_ERROR([AE_BLOCKING_PARSER doesnt point to a valid executable])
    fi
else
    AC_CHECK_PROG(HAVE_PARSER_ON_PATH, ae-blocking-parser, yes, no,)
    if test "$HAVE_PARSER_ON_PATH" = "no"; then
        AC_MSG_ERROR([could not find a valid ae-blocking-parser program])
    fi
fi

dnl Check to see if CC can handle label pointers
AC_MSG_CHECKING(if the C compiler supports label pointers)
AC_TRY_COMPILE([], [void* foo; my_label: foo=&&my_label;],
    AC_MSG_RESULT(yes),
    AC_MSG_RESULT(no)
    AC_MSG_ERROR(CC does not support label pointer extension, please try gcc or clang)
)

dnl regardless of what compiler the user is using, we still check for gcc in 
dnl the path to use for generating dependencies
AC_CHECK_PROG(HAVE_GCC_BIN, gcc, yes, no,)
if test "$HAVE_GCC_BIN" = "no"; then
        AC_MSG_ERROR([gcc must be in path in order to generate build dependencies])
fi

dnl force use of POSIX locking in triton-thread header; the aesop library
dnl code is not safe otherwise
CFLAGS="$CFLAGS -D__TRITON_POSIX_LOCKING__"
LIBS="$LIBS -lpthread"

dnl ====================================================
dnl Use pkgconfig to find c-utils
dnl  - Sets C_UTILS_CFLAGS, C_UTILS_LIBS
dnl ====================================================
PKG_CHECK_MODULES([C_UTILS],[c-utils], [],
   [AC_MSG_ERROR([Could not find working c-utils installation!])])
LIBS="$C_UTILS_LIBS $LIBS"
CFLAGS="$C_UTILS_CFLAGS $CFLAGS"
dnl unfortunately pkg-config doesn't separate cppflags/cflags
CPPFLAGS="$C_UTILS_CPPFLAGS $CPPFLAGS"


dnl ====================================================
dnl = Find timeout program to safely run tests for now =
dnl ====================================================
TIMEOUT_PROG=
AC_CHECK_PROGS(TIMEOUT_PROG,[timeout])
if test -z "$TIMEOUT_PROG"; then
   AC_MSG_ERROR("Could not find 'timeout' program!")
fi
AC_SUBST(TIMEOUT_PROG)

dnl =========================================================================
dnl ==== Check for libev ====================================================
dnl =========================================================================

AC_ARG_WITH([libev],
            [AS_HELP_STRING([[--with-libev=[DIR]]], [Use libev from specified directory])],
            [
                CPPFLAGS="$CPPFLAGS -I${with_libev}/include"
                # note: add rpath too because stock libev install uses
                # shared libs
                LDFLAGS="$LDFLAGS -Wl,-rpath=${with_libev}/lib -L${with_libev}/lib"
                AESOP_PKGCONFIG_LIBS="${AESOP_PKGCONFIG_LIBS} -L${with_libev}/lib -lev -lm"
            ],
            [
                AESOP_PKGCONFIG_LIBS="${AESOP_PKGCONFIG_LIBS} -lev -lm"
            ]
            )

AC_CHECK_HEADER([ev.h],[],
         [AC_MSG_ERROR([Could not find libev header in
          ${with_libev}/include!])])

AC_CHECK_LIB([ev],[ev_once],[evok=1; LIBS="-lev $LIBS"],
             [evok=0])
if test "A$evok" == "A0"; then
   AC_CHECK_LIB([m], [log], [], [])
   dnl Needs to be a different function to avoid hitting cache
   AC_CHECK_LIB([ev], [ev_break], [],
             [AC_MSG_ERROR([Could not find useable
           libev library at prefix ${with_libev}/lib!])])
fi


dnl =========================================================================
dnl == OpenPA ===============================================================
dnl =========================================================================

dnl ====================================================
dnl Use pkgconfig to find OpenPA
dnl  - Sets OPENPA_CFLAGS, OPENPA_LIBS
dnl ====================================================

PKG_CHECK_MODULES([OPENPA],[openpa], [],
   [AC_MSG_ERROR([Could not find working openpa installation!])])
LIBS="$OPENPA_LIBS $LIBS"
CFLAGS="$OPENPA_CFLAGS $CFLAGS"
dnl unfortunately pkg-config doesn't separate cppflags/cflags
CPPFLAGS="$OPENPA_CPPFLAGS $CPPFLAGS"

dnl ====================================================
dnl Use pkgconfig to find Seagate Kinetic Libraries
dnl  - optional for Aesop build
dnl  - Sets KINETIC_CFLAGS, KINETC_LIBS
dnl ====================================================
PKG_CHECK_MODULES([KINETIC],[kinetic],
                  [[KINETIC_ENABLED=1]
                   [KINETIC_PKG=kinetic]],
                  [AC_DEFINE([KINETIC_ENABLED], [0], [Disables aekinetic code])])
LIBS="$KINETIC_LIBS $LIBS"
CFLAGS="$KINETIC_CFLAGS $CFLAGS"
CPPFLAGS="$KINETIC_CPPFLAGS $CPPFLAGS"
AM_CONDITIONAL([KINETIC_ENABLED], [test x$KINETIC_ENABLED = x1])
AC_SUBST(KINETIC_ENABLED)
AC_SUBST(KINETIC_PKG)

BUILD_ABSOLUTE_TOP=${PWD}
SRC_RELATIVE_TOP=$srcdir
SRC_ABSOLUTE_TOP=`cd $srcdir; pwd`

AC_SUBST(BUILD_ABSOLUTE_TOP)
AC_SUBST(SRC_RELATIVE_TOP)
AC_SUBST(SRC_ABSOLUTE_TOP)

AC_SUBST(CFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(LIBS)
AC_SUBST(CPPFLAGS)

STRICT_CFLAGS=1
AC_ARG_ENABLE(strict,
[  --disable-strict	Turn off strict compiler warnings],
[if test "x$enableval" = "xno" ; then
    STRICT_CFLAGS=0
fi],
[
    STRICT_CFLAGS=1
])
AC_SUBST(STRICT_CFLAGS)

AE_DEBUG=
AC_ARG_ENABLE(aesop-debug,
[  --enable-aesop-debug	Turn on debugging of the aesop-parser],
    AE_DEBUG=1)
AC_SUBST(AE_DEBUG)

AC_ARG_ENABLE(release,
        [AS_HELP_STRING(
                [--enable-release],
                [Optimize for performance; disable debugging])
        ],
        [enable_release=$enableval],
        [enable_release="no"])

if test "A$enable_release" = "Ayes"; then
   CPPFLAGS="-DNDEBUG $CPPFLAGS"
elif test "A$enable_release" = "Ano"; then
   dummy=""
else
   AC_MSG_ERROR([Invalid value for --enable-release! must be yes or no])
fi

AC_ARG_ENABLE(coverage,
[  --enable-coverage	Enable coverage analysis (disables optimizations)],
[  CFLAGS="$CFLAGS -g -pg -ftest-coverage -O0"
   ENABLE_COVERAGE=1],
)
AC_SUBST(ENABLE_COVERAGE)


dnl check for optional berkeley db support
dnl
AC_ARG_WITH(db,
       [  --with-db=<dir>         Location of installed DB package
(default=/usr)],
       [AX_BERKELEY_DB(${withval})],
       [AX_BERKELEY_DB("")])
CFLAGS="$DB_CFLAGS $CFLAGS"
CPPFLAGS="$DB_CFLAGS $CPPFLAGS"

AC_COMPILE_IFELSE(
[#include <stdlib.h>
#ifdef __BLOCKS__
#error "OSX defines __BLOCKS__ and includes then aren't ANSI C"
#endif
],
[BLOCKSOPT=""],
[BLOCKSOPT="-fno-blocks"])

AC_SUBST(BLOCKSOPT)

AC_MSG_CHECKING(for syncfs)
AC_TRY_LINK(
    [#define _GNU_SOURCE
     #include <unistd.h>
    ],
    [
     syncfs(0);
    ],
    AC_MSG_RESULT(yes)
    AC_DEFINE(HAVE_SYNCFS, 1, Define if syncfs function is present)
    ,
    AC_MSG_RESULT(no)
)

AC_MSG_CHECKING(for fdatasync)
AC_TRY_LINK(
    [
     #include <unistd.h>
    ],
    [
     fdatasync(0);
    ],
    AC_MSG_RESULT(yes)
    AC_DEFINE(HAVE_FDATASYNC, 1, Define if fdatasync function is present)
    ,
    AC_MSG_RESULT(no)
)

AC_MSG_CHECKING(for fallocate)
AC_TRY_LINK(
    [#define _GNU_SOURCE
     #include <fcntl.h>
    ],
    [
     fallocate(0, 0, 0, 0);
    ],
    AC_MSG_RESULT(yes)
    AC_DEFINE(HAVE_FALLOCATE, 1, Define if fallocate function is present)
    ,
    AC_MSG_RESULT(no)
)

# TODO: clean up how CFLAGS are handled by AECC; this is confusing
# these are cflags that will be automatically applied by aecc
AECC_CFLAGS=""
AC_SUBST(AECC_CFLAGS)

AESOP_PKGCONFIG_LIBS="${AESOP_PKGCONFIG_LIBS} -lpthread"
AC_SUBST(AESOP_PKGCONFIG_LIBS)

AC_CONFIG_FILES([bin/aecc-utils], [])
AC_CONFIG_FILES([bin/aecc], [chmod +x bin/aecc])

AC_CONFIG_FILES([tests/testrunner.sh], [chmod +x tests/testrunner.sh])
AC_CONFIG_FILES([Makefile
maint/aesop.pc])

AC_OUTPUT
