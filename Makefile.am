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

AM_CPPFLAGS = -I$(top_srcdir)

# So code can include aesop/... headers whether installed or in-tree
AM_CPPFLAGS += -I$(top_srcdir)/include


AM_CFLAGS =

AM_LIBS =

AM_CXXFLAGS = $(AM_CFLAGS)

AM_GHCFLAGS =


include Make.rules

include $(top_srcdir)/Makefile.subdir


