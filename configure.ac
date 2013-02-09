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
AC_INIT(aesop, 0.1.1, aesop-dev@mcs.anl.gov)

AC_SUBST(AESOP_VERSION)

AC_DEFINE_UNQUOTED(AESOP_VERSION_MAJOR, $AESOP_MAJOR, major version number)
AC_DEFINE_UNQUOTED(AESOP_VERSION_MINOR, $AESOP_MINOR, minor version number)
AC_DEFINE_UNQUOTED(AESOP_VERSION_SUB,   $AESOP_SUB, sub version number)

AC_CONFIG_HEADER(aesop-config.h)
AC_CONFIG_AUX_DIR(maint/config)

AC_CONFIG_SUBDIRS(src/c-utils)

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

dnl ======================================================================
dnl  Try harder to be valgrind safe
dnl ======================================================================
AC_ARG_ENABLE(valgrind-clean,
        [AS_HELP_STRING(
                [--enable-valgrind-clean],
                [Try harder to avoid valgrind warnings])
        ])

VALGRIND_CLEAN=0
AS_IF([test "x$enable_valgrind_clean" = "xyes"], [
       VALGRIND_CLEAN=1
])
AC_SUBST(VALGRIND_CLEAN)


dnl =========================================================================
dnl == OpenPA ===============================================================
dnl =========================================================================

# OpenPA
AC_ARG_WITH([openpa],
            [AS_HELP_STRING([[--with-openpa[=DIR]]],
                             [rather than the one included in src/openpa.  Pass
                             "embedded" to force usage of the OpenPA source
                             distributed with triton.])],
            [],
            [
             OLDLIBS="$LIBS"
             LIBS="-lopa $LIBS"
             AC_LINK_IFELSE([AC_LANG_PROGRAM([dnl
#include "opa_primitives.h"
],[
OPA_int_t i;
OPA_store_int(&i,10);
OPA_fetch_and_incr_int(&i);
])dnl
                             ],
                            [with_openpa=system],
                            [with_openpa="/usr"])
             LIBS="$OLDLIBS"
             ])

opadir=""
AC_SUBST([opadir])

if test "$with_openpa" = "embedded" ; then
   AC_MSG_ERROR([Embedded mode disabled for OpenPA!])
    if test -e "${srcdir}/extern/openpa" ; then
        opadir="openpa"
        EXTERNAL_SRC_DIRS="$EXTERNAL_SRC_DIRS extern/openpa"
        CPPFLAGS="-I${srcdir}/extern/openpa/src $CPPFLAGS"
        AC_CONFIG_SUBDIRS([extern/openpa])
        LIBS="-lopa $LIBS"
    else
        AC_MSG_WARN([Attempted to use the embedded OpenPA source tree in dnl
                     "${srcdir}/extern/openpa", but it is missing.
                     Configuration or dnl
                     compilation may fail later.]) dnl
    fi
elif test "$with_openpa" = "system" ; then
   dummy="dummy";
   dnl LIBS="-lopa $LIBS"
elif test "$with_openpa" = "no" ; then
   AC_MSG_ERROR([OpenPA is required!])
else
    # The user specified an already-installed OPA; just sanity check, don't
    # subconfigure it
    AS_IF([test -s "${with_openpa}/include/opa_primitives.h" -a -s "${with_openpa}/include/opa_config.h"],
          [:],[AC_MSG_ERROR([the OpenPA installation in "${with_openpa}" appears broken])])
    CPPFLAGS="-I${with_openpa}/include $CPPFLAGS"
dnl CPPFLAGS="-I${ac_top_build_prefix}/extern/openpa"
dnl    LIBS="-lopa $LIBS"
    LDFLAGS="-L${with_openpa}/lib $LDFLAGS"
fi


AC_CHECK_LIB([opa],[OPA_Queue_init],[],[AC_MSG_ERROR([Could not find useable
              OpenPA library!])])

dnl AC_SUBST([subdirs])

dnl =========================================================================
dnl == Google's PerfTools ===================================================
dnl =========================================================================

AC_ARG_WITH(perftools,
    AS_HELP_STRING([--with-perftools=dir],
                   [Location of google perftools installation]),
    PERFTOOL_DIR="$withval",PERFTOOL_DIR="")

if test "A$PERFTOOL_DIR" == "Ano"
then
   use_perftools=0
   PERFTOOL_DIR=""
elif test "A$PERFTOOL_DIR" == "Ayes"
then
   use_perftools=1
   PERFTOOL_DIR=""
elif test  "A$PERFTOOL_DIR" != "A"
then
   use_perftools=1
else
dnl 2 means autodetect
   use_perftools=2
fi

if test -n "$PERFTOOL_DIR"
then
   CPPFLAGS="$CPPFLAGS -I${PERFTOOL_DIR}/include"
   LDFLAGS="$LDFLAGS -L${PERFTOOL_DIR}/lib"
fi

if test "A$use_perftools" != "A0"
then
   perftools_found=1
   AC_CHECK_HEADERS([google/tcmalloc.h],[],
                    [perftools_found=])

   AC_CHECK_LIB([tcmalloc],[tc_new],[],[perftools_found=])
   AC_CHECK_LIB([profiler],[ProfilerStart], [], [perftools_found=])
else
   perftools_found=0
fi

AC_MSG_CHECKING([if Google Perftools are used])
if test "x${perftools_found}" == "x1"
then
   AC_DEFINE([HAVE_PERFTOOLS], [1], [If google perftools are available])
   AC_MSG_RESULT([yes])
else
   AC_MSG_RESULT([no])
   if test "A$use_perftools" == "A1"; then
         AC_MSG_ERROR([Perftools requested but could not be found... Aborting])
   fi
fi

dnl ==========================================================================
dnl ==== SSM =================================================================
dnl ==========================================================================
AC_ARG_WITH([ssm],
            [AS_HELP_STRING([--with-ssm@<:@=DIR@:>@], [location of SSM])],
            [BUILD_SSM=1
             SSMPATH="$withval"],
            [BUILD_SSM=0])
AC_SUBST(SSMPATH)
AC_SUBST(BUILD_SSM)

dnl ==========================================================================
dnl ==== Documentation =======================================================
dnl ==========================================================================

AC_ARG_VAR([A2X], [Asciidoc a2x tool])
AC_CHECK_PROGS([A2X], [a2x a2x.py], [])
AC_CHECK_PROGS([ASCIIDOC], [asciidoc asciidoc.py], [])


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

AC_ARG_ENABLE(coverage,
[  --enable-coverage	Enable coverage analysis (disables optimizations)],
[  CFLAGS="$CFLAGS -g -pg -ftest-coverage -O0"
   ENABLE_COVERAGE=1],
)
AC_SUBST(ENABLE_COVERAGE)

dnl a mechanism to turn off threads in the client library
AC_MSG_CHECKING([for client library thread safety support])
AC_ARG_ENABLE(thread-safety,
[  --disable-thread-safety Disables thread safety in the client library],
[if test "x$enableval" = "xno" ; then
    LIBCFLAGS="$LIBCFLAGS -D__TRITON_NULL_LOCKING__"
    THREAD_LIB=""
    AC_MSG_RESULT(no)
fi],
[   LIBCFLAGS="$LIBCFLAGS -D__TRITON_POSIX_LOCKING__"
    THREAD_LIB="-lpthread"
    AC_MSG_RESULT(yes) ])

dnl check for optional berkeley db support
dnl
AC_ARG_WITH(db,
       [  --with-db=<dir>         Location of installed DB package
(default=/usr)],
       [AX_BERKELEY_DB(${withval})],
       [AX_BERKELEY_DB("")])

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

AC_CONFIG_FILES([bin/aecc-utils], [])
AC_CONFIG_FILES([bin/aecc], [chmod +x bin/aecc])

AC_OUTPUT([Makefile
resources/worker/module.mk
resources/worker/test/module.mk
resources/sem/module.mk
resources/sem/test/module.mk
resources/thread/module.mk
resources/thread/test/module.mk
src/aefile/module.mk
src/aebdb/module.mk
src/aebdb/test/module.mk
src/aefile/test/module.mk
src/resourcebuilder/module.mk
src/resourcebuilder/test/module.mk
src/socket/module.mk
src/socket/test/module.mk
src/ssm/module.mk
src/ssm/test/module.mk
src/timer/test/module.mk
src/timer/module.mk
module.mk
tests/module.mk
parser/module.mk
parser/tests/blocking/module.mk
doc/module.mk
])

