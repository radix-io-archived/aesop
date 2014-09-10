AUTOMAKE_OPTIONS =  foreign -Wno-override subdir-objects 
ACLOCAL_AMFLAGS = -I maint/config

SUFFIXES =

bin_PROGRAMS =
bin_SCRIPTS =
noinst_LIBRARIES =
noinst_PROGRAMS =
lib_LIBRARIES =
lib_LTLIBRARIES =
noinst_HEADERS =
TESTS =
XFAIL_TESTS =
check_PROGRAMS =
EXTRA_PROGRAMS =
CLEANFILES = $(bin_SCRIPTS)
MAINTAINERCLEANFILES =
EXTRA_DIST = 
BUILT_SOURCES =
AM_CTAGSFLAGS=--excmd=pattern -B --extra=+f --langmap=c:+.ae.hae -I pwait,pbranch

EXTRA_DIST += prepare


# Extra flags for linking aesop programs against the in-tree libs
AESOP_LOCAL_LIBS = -L$(top_builddir)/lib $(AESOP_PKGCONFIG_LIBS)

AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_srcdir) -Iinclude
AM_CFLAGS =
AM_LIBS =
AM_CXXFLAGS = $(AM_CFLAGS)
AM_GHCFLAGS =
AE_SRC = 
AE_HDR =
AM_DEFAULT_SOURCE_EXT = .ae

AECC = bin/aecc

#
# pkgconfig
#
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = maint/aesop.pc

lib_LIBRARIES += src/libaesop.a
src_libaesop_a_SOURCES = 
LDADD = src/libaesop.a

include Make.rules
include $(top_srcdir)/src/Makefile.subdir
include $(top_srcdir)/src/components/Makefile.subdir
include $(top_srcdir)/src/resources/Makefile.subdir
include $(top_srcdir)/tests/Makefile.subdir
include $(top_srcdir)/tests/resources/Makefile.subdir

-include $(AE_SRC:.ae=.ae.d)
-include $(AE_HDR:.hae=.hae.d)

