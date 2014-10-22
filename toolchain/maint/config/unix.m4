
AC_DEFUN([AX_UNIX],
[

     dnl figure out the library type
     dnl Darwin/OSX doesn't allow static linking
     dnl and dynamic libraries are .dylib files rather
     dnl than shared objects
     AS_CASE([$host],
       [darwin*-*-*], [USE_DYLIBS=yes],
       [USE_DYLIBS=""]
     )
     AC_SUBST([USE_DYLIBS])

    dnl Need to check for O_DIRECT, doesn't exist on Darwin/OSX
    AC_MSG_CHECKING(for O_DIRECT)
    AC_COMPILE_IFELSE([
#define _GNU_SOURCE
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>

void testodirect(void)
{
    open(NULL, O_DIRECT);
}
],
AC_MSG_RESULT(yes)
AC_DEFINE(HAVE_ODIRECT, 1, Define if O_DIRECT exists),
AC_MSG_RESULT(no))

dnl Need to check for O_NOATIME, doesn't exist on Darwin/OSX
AC_MSG_CHECKING(for O_NOATIME)
AC_COMPILE_IFELSE([
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

void testnoatime(void)
{
    open(NULL, O_NOATIME);
}
],
AC_MSG_RESULT(yes)
AC_DEFINE(HAVE_NOATIME, 1, Define if O_NOATIME exists),
AC_MSG_RESULT(no))

dnl OSX >= 10.5 has blocks support, which defaults to including some nasty non-ANSI C code in standard headers
AC_MSG_CHECKING([for OS X blocks support])
AC_COMPILE_IFELSE(
[#include <stdlib.h>
#ifdef __BLOCKS__
#error "OSX defines __BLOCKS__ and includes that aren't ANSI C"
#endif
],
[AC_MSG_RESULT(no)], 
[AC_MSG_RESULT(yes)
CFLAGS="$CFLAGS -fno-blocks"])


])
