#
# Copyright (c) 2012-2015 LAAS/CNRS
# All rights reserved.
#
# Permission to use, copy, modify, and distribute this software for any purpose
# with or without   fee is hereby granted, provided   that the above  copyright
# notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
# REGARD TO THIS  SOFTWARE INCLUDING ALL  IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS. IN NO EVENT SHALL THE AUTHOR  BE LIABLE FOR ANY SPECIAL, DIRECT,
# INDIRECT, OR CONSEQUENTIAL DAMAGES OR  ANY DAMAGES WHATSOEVER RESULTING  FROM
# LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
# OTHER TORTIOUS ACTION,   ARISING OUT OF OR IN    CONNECTION WITH THE USE   OR
# PERFORMANCE OF THIS SOFTWARE.
#
#                                            Anthony Mallet on Tue Feb 27 2012
#
ACLOCAL_AMFLAGS=-I autoconf

fiacredir=${prefix}/${genom3sitedir}/fiacre

# --- c client -------------------------------------------------------------

fiacreclientdir=${fiacredir}
nobase_dist_fiacreclient_DATA=\
	model/template.tcl		\
	common/typeutil.tcl			\
						\
	model/bootstrap.sh		\
	model/model.Makefile.am	\
	model/model.configure.ac	\
						\
	model/fiacre.tcl


.PHONY: dist-robotpkg git-tag

dist_name = @PACKAGE_TARNAME@-@PACKAGE_VERSION@

git-tag:
	git tag -m "release @PACKAGE_VERSION@"  $(dist_name)
	echo "Do not forget to push the tag with git push --tags"

dist-robotpkg: distcheck git-tag
	scp $(dist_name).tar.gz cricket.laas.fr:/usr/local/openrobots/distfiles/genom3-fiacre/

