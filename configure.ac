#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.61])
AC_INIT([mame], [0.0.1])
AC_CONFIG_SRCDIR([mame.h])
# AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([limits.h malloc.h stdlib.h string.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE
AC_TYPE_INT32_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_STRTOD

AC_CHECK_FUNC(floor,,AC_CHECK_LIB(m,floor,LIBS=["$LIBS -lm"]))
AC_CHECK_FUNCS([atexit floor ceil isascii memmove memset strtol])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
