AUTOMAKE_OPTIONS =  foreign -Wno-override subdir-objects 
ACLOCAL_AMFLAGS = -I maint/config

SUFFIXES =

bin_PROGRAMS =
bin_SCRIPTS = bin/aecc bin/aecc-utils
noinst_LTLIBRARIES =
noinst_PROGRAMS =
lib_LTLIBRARIES =
noinst_HEADERS =
pkginclude_HEADERS = 
TESTS =
XFAIL_TESTS =
check_PROGRAMS =
EXTRA_PROGRAMS =
CLEANFILES =
MAINTAINERCLEANFILES =
EXTRA_DIST = 
BUILT_SOURCES =
AM_CTAGSFLAGS=--excmd=pattern -B --extra=+f --langmap=c:+.ae.hae -I pwait,pbranch

EXTRA_DIST += prepare maint/depend-ae.sh COPYRIGHT doc

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

lib_LTLIBRARIES += src/libaesop.la
src_libaesop_la_SOURCES = 
LDADD = src/libaesop.la

include Make.rules
include $(top_srcdir)/src/Makefile.subdir
include $(top_srcdir)/src/components/Makefile.subdir
include $(top_srcdir)/src/resources/Makefile.subdir
include $(top_srcdir)/tests/Makefile.subdir
include $(top_srcdir)/tests/resources/Makefile.subdir
include $(top_srcdir)/tests/components/Makefile.subdir
include $(top_srcdir)/tests/parser/Makefile.subdir
include $(top_srcdir)/include/aesop/Makefile.subdir
-include $(AE_SRC:.ae=.ae.d)
-include $(AE_HDR:.hae=.hae.d)

BUILT_SOURCES += $(AE_HDR:.hae=.h)
