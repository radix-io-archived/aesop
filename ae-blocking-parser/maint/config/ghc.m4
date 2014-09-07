AC_DEFUN([AX_CHECK_GHCPKG],
[
    GHCPKG=$1
    pkgname=$2
    AC_MSG_CHECKING([for valid ghc $pkgname module..])
    res=`$GHCPKG find-module --simple-output $pkgname`
    if test -z "$res"; then
        AC_MSG_ERROR(no.  Please run the scripts setup-hs-local and setup-langc in maint/hs/.)
    fi
    AC_MSG_RESULT(yes)
])

AC_DEFUN([AX_GHC],
[    
    if test -f .bindist; then
        AC_MSG_RESULT(skipping; binary distribution doesn't need ghc)
    else
        AC_ARG_WITH(ghc,
           [  --with-ghc=<dir>       Location of installed ghc],
           [AC_PATH_PROG(GHC, ${withval}/bin/ghc)
            AC_PATH_PROG(GHCPKG, ghc-pkg, ${withval}/bin/ghc-pkg)],
           [AC_PATH_PROG(GHC, ghc)
            AC_PATH_PROG(GHCPKG, ghc-pkg)])
        AC_SUBST(GHC)
        AC_SUBST(GHCPKG)

        AX_CHECK_GHCPKG($GHCPKG, Language.C)
        AX_CHECK_GHCPKG($GHCPKG, Text.Regex.PCRE)
    fi
])
