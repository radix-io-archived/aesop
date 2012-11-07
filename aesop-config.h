/* aesop-config.h.  Generated from aesop-config.h.in by configure.  */
/* aesop-config.h.in.  Generated from configure.ac by autoheader.  */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* major version number */
#define AESOP_VERSION_MAJOR 

/* minor version number */
#define AESOP_VERSION_MINOR 

/* sub version number */
#define AESOP_VERSION_SUB 

/* Define to 1 if you have the <google/tcmalloc.h> header file. */
/* #undef HAVE_GOOGLE_TCMALLOC_H */

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `opa' library (-lopa). */
#define HAVE_LIBOPA 1

/* Define to 1 if you have the `profiler' library (-lprofiler). */
/* #undef HAVE_LIBPROFILER */

/* Define to 1 if you have the `tcmalloc' library (-ltcmalloc). */
/* #undef HAVE_LIBTCMALLOC */

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define if O_NOATIME exists */
/* #undef HAVE_NOATIME */

/* Define if O_DIRECT exists */
/* #undef HAVE_ODIRECT */

/* If google perftools are available */
/* #undef HAVE_PERFTOOLS */

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "aesop-dev@mcs.anl.gov"

/* Define to the full name of this package. */
#define PACKAGE_NAME "aesop"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "aesop 0.1.1"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "aesop"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "0.1.1"

/* The size of `long int', as computed by sizeof. */
#define SIZEOF_LONG_INT 4

/* The size of `void *', as computed by sizeof. */
#define SIZEOF_VOID_P 4

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif
