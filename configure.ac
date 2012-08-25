#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# configure.ac - configure script template for Letterfall
# Copyright (C) 2012 Alan Manuel K. Gloria
#
# This file is part of Letterfall.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, without even the implied warranty of
# MERCHANTIBILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>

AC_PREREQ([2.68])
AC_INIT([letterfall], [1.0.0], [almkglor@gmail.com])
AC_CONFIG_AUX_DIR([auxdir])

AM_INIT_AUTOMAKE([-Wall gnu])

AC_CONFIG_SRCDIR([letterfall/main.sscm])

# Checks for programs.
# Don't use GUILE_PROGS.  GUILE_PROGS requires guile-config
# to exist, and on a few systems (such as my workplace's
# system...) there's a guile but not a guile-config
AC_PATH_PROG(GUILE_PLAIN, guile)
AC_PATH_PROG(GUILE_18, guile-1.8) # prefer versioned names
AC_PATH_PROG(GUILE_20, guile-2.0) # prefer versioned names

# Check for guile-gnome-2 library
AC_MSG_CHECKING([if guile-1.8 can use guile-gnome-2])
AS_IF([test x"$GUILE_18" = "x"],[#then
  GUILE_18_POSSIBLE=no
],[#else
  AS_IF([$GUILE_18 -q -s "$srcdir/guile-gnome-test.scm" >/dev/null 2>&1],[#then
    GUILE_18_POSSIBLE=yes
  ],[#else
    GUILE_18_POSSIBLE=no
  ])
])
AC_MSG_RESULT([$GUILE_18_POSSIBLE])
AC_MSG_CHECKING([if guile-2.0 can use guile-gnome-2])
AS_IF([test x"$GUILE_20" = "x"],[#then
  GUILE_20_POSSIBLE=no
],[#else
  AS_IF([$GUILE_20 -q -s "$srcdir/guile-gnome-test.scm" >/dev/null 2>&1],[#then
    GUILE_20_POSSIBLE=yes
  ],[#else
    GUILE_20_POSSIBLE=no
  ])
])
AC_MSG_RESULT([$GUILE_20_POSSIBLE])
AC_MSG_CHECKING([if guile can use guile-gnome-2])
AS_IF([test x"$GUILE_PLAIN" = "x"],[#then
  GUILE_PLAIN_POSSIBLE=no
],[#else
  AS_IF([$GUILE_PLAIN -q -s "$srcdir/guile-gnome-test.scm" >/dev/null 2>&1],[#then
    GUILE_PLAIN_POSSIBLE=yes
  ],[#else
    GUILE_PLAIN_POSSIBLE=no
  ])
])
AC_MSG_RESULT([$GUILE_PLAIN_POSSIBLE])

# select one, prefer the versioned ones first
AS_IF([test x"$GUILE_20_POSSIBLE" = xyes],[#then
  GUILE=$GUILE_20
],[#else
  AS_IF([test x"$GUILE_18_POSSIBLE" = xyes],[#then
    GUILE=$GUILE_18
  ],[#else
    AS_IF([test x"$GUILE_PLAIN_POSSIBLE" = xyes],[#then
      GUILE=$GUILE_PLAIN
    ],[#else
      AC_MSG_ERROR([guile or guile-gnome-2 not found or not compatible])
    ])
  ])
])

AC_SUBST(GUILE)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
