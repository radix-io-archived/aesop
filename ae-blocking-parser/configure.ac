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
AC_INIT(aesop, 0.1.2, aesop-dev@mcs.anl.gov)

AC_SUBST(AESOP_VERSION)

AC_DEFINE_UNQUOTED(AESOP_VERSION_MAJOR, $AESOP_MAJOR, major version number)
AC_DEFINE_UNQUOTED(AESOP_VERSION_MINOR, $AESOP_MINOR, minor version number)
AC_DEFINE_UNQUOTED(AESOP_VERSION_SUB,   $AESOP_SUB, sub version number)

AC_CONFIG_HEADER(aesop-config.h)
AC_CONFIG_AUX_DIR(maint/config)

AC_PROG_INSTALL
AC_PROG_CC
AC_PROG_CPP

AC_C_BIGENDIAN

AC_CHECK_SIZEOF([long int])
AC_CHECK_SIZEOF(void *)

AC_MSG_CHECKING([for required gcc])
if test "x$GCC" = "x"; then
	AC_MSG_ERROR([no])
fi
AC_MSG_RESULT(yes)

dnl
dnl Verify pkg-config
dnl
PKG_PROG_PKG_CONFIG

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

AX_GHC("")


dnl ==========================================================================
dnl check for unix variants
dnl ==========================================================================
AX_UNIX("")

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

AC_ARG_ENABLE(verbose,
[  --enable-verbose     Enables verbose output during build process],
QUIET_COMPILE=0,
QUIET_COMPILE=1)
AC_SUBST(QUIET_COMPILE)

AC_ARG_ENABLE(error-on-warning,
[  --enable-error-on-warning     Generate compiler error on warnings],
WARNINGS_AS_ERRORS=1,
WARNINGS_AS_ERRORS=0)
AC_SUBST(WARNINGS_AS_ERRORS)

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

AC_COMPILE_IFELSE(
[#include <stdlib.h>
#ifdef __BLOCKS__
#error "OSX defines __BLOCKS__ and includes then aren't ANSI C"
#endif
],
[BLOCKSOPT=""],
[BLOCKSOPT="-fno-blocks"])

AC_SUBST(BLOCKSOPT)

# these are cflags that will be automatically applied by aecc
AECC_CFLAGS=$LIBCFLAGS
AC_SUBST(AECC_CFLAGS)

AC_SUBST(LIBCFLAGS)
AC_SUBST(THREAD_LIB)
AESOP_PKGCONFIG_LIBS="${AESOP_PKGCONFIG_LIBS} ${THREAD_LIB}"
AC_SUBST(AESOP_PKGCONFIG_LIBS)

AC_OUTPUT([Makefile
module.mk
src/module.mk
maint/ae-blocking-parser.pc
])

