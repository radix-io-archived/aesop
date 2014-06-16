AUTOMAKE_OPTIONS =  foreign -Wno-override subdir-objects 
ACLOCAL_AMFLAGS = -I m4

SUFFIXES =

bin_PROGRAMS =
bin_SCRIPTS =
noinst_LIBRARIES =
noinst_PROGRAMS =
lib_LIBRARIES =
lib_LTLIBRARIES =
noinst_HEADERS =
TESTS =
check_PROGRAMS =
EXTRA_PROGRAMS =
CLEANFILES = $(bin_SCRIPTS)
MAINTAINERCLEANFILES =
EXTRA_DIST =
BUILT_SOURCES =
AM_CTAGSFLAGS=--excmd=pattern -B --extra=+f --langmap=c:+.ae.hae -I pwait,pbranch


# List of dependencies for _the use_ of the aecc compiler _in tree_
compilerdeps =

# Extra flags for linking aesop programs against the in-tree libs
AESOP_LOCAL_LIBS = -L$(top_builddir)/lib $(AESOP_PKGCONFIG_LIBS)

AM_CPPFLAGS = -I$(top_srcdir)

# So code can include aesop/... headers whether installed or in-tree
AM_CPPFLAGS += -I$(top_srcdir)/include -I$(top_builddir)/include


AM_CFLAGS =

AM_LIBS =

AM_CXXFLAGS = $(AM_CFLAGS)

AM_GHCFLAGS =


#
# pkgconfig
#
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = aesop.pc
$(pkgconfig_DATA): config.status


include Make.rules

include $(top_srcdir)/Makefile.subdir


