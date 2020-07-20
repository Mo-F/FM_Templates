#
# Copyright (c) 2012-2015 LAAS/CNRS
#
# GenoM3/fiacre template autoconf file
#
# heavily inspired by the genom3 pocolibs template, etc.
#

AC_PREREQ(2.62)
AC_INIT([genom3-fiacre],[1.0.0],[mfoughal@laas.fr])

AC_CONFIG_AUX_DIR([autoconf])
AC_CONFIG_MACRO_DIR([autoconf])
AM_INIT_AUTOMAKE([foreign no-define])
AC_PROG_INSTALL


# external software
PKG_CHECK_MODULES(genom3, [genom3 >= 2.99.20])
_PKG_CONFIG(genom3prefix, [variable=prefix], [genom3 >= 2.99.20])
_PKG_CONFIG(genom3sitedir, [variable=sitetmpldir], [genom3 >= 2.99.20])
AC_SUBST(genom3sitedir, [${pkg_cv_genom3sitedir##$pkg_cv_genom3prefix/}])

if test "x$prefix" = xNONE; then
  AC_MSG_CHECKING([default prefix...])
  prefix=$pkg_cv_genom3prefix
  AC_MSG_RESULT([$prefix])
fi

# output
AC_CONFIG_FILES([
	Makefile
])
AC_OUTPUT
