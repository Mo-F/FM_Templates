#
# Copyright (c) 2012-2015 LAAS/CNRS
# All rights reserved.
#
# Redistribution  and  use  in  source  and binary  forms,  with  or  without
# modification, are permitted provided that the following conditions are met:
#
#   1. Redistributions of  source  code must retain the  above copyright
#      notice and this list of conditions.
#   2. Redistributions in binary form must reproduce the above copyright
#      notice and  this list of  conditions in the  documentation and/or
#      other materials provided with the distribution.
#
# THE SOFTWARE  IS PROVIDED "AS IS"  AND THE AUTHOR  DISCLAIMS ALL WARRANTIES
# WITH  REGARD   TO  THIS  SOFTWARE  INCLUDING  ALL   IMPLIED  WARRANTIES  OF
# MERCHANTABILITY AND  FITNESS.  IN NO EVENT  SHALL THE AUTHOR  BE LIABLE FOR
# ANY  SPECIAL, DIRECT,  INDIRECT, OR  CONSEQUENTIAL DAMAGES  OR  ANY DAMAGES
# WHATSOEVER  RESULTING FROM  LOSS OF  USE, DATA  OR PROFITS,  WHETHER  IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR  OTHER TORTIOUS ACTION, ARISING OUT OF OR
# IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
#
#                                           Anthony Mallet on Mon Mar 12 2012
#

# Fiacre model

template usage {*}{
    "\n"
    "Fiacre model of the components.\n"
    "\n"
    "This template generates a Fiacre model for a GenoM component.\n"
    "\n"
    "Supported options:\n"
    "  -C, --directory=dir\toutput files in dir\n"
    "  -p, --preserve\tdo not overwrite existing files\n"
    "  -h, --help\t\tprint usage summary (this text)"
    "  -pn. --number-of-cores=n user-defined number of available processor cores for their application"
}

# defaults
set odir model/fiacre
engine mode +overwrite +move-if-change
set corenumb 0

# parse options
template options {
    -C - --directory	{ set odir [template arg] }
    -p - --preserve	{ engine mode -overwrite }
    -h - --help		{ puts [template usage]; exit 0 }
    -pn - --number-of-cores { set corenumb [template arg] }
}

# add common services to dotgen specification
dotgen parse file [file join [dotgen template builtindir] common/genom.gen]

# parse input
set input [list]
foreach f $argv {
  dotgen parse file $f
  lappend input [file normalize $f]
}

# only one component
set comp [dotgen components]
#if {[llength $comp] > 1} {
#  template fatal "component compositing not supported"
#}
engine chdir $odir

# require utility procs
template require ../common/typeutil.tcl

#set types [$comp types public]

set lisp_header { ;;; <"[--- Generated by [dotgen genom version]. Do not edit - ]">}

# common header for all files (copyright info from .gen file)
set header {/* <"[--- Generated by [dotgen genom version]. Do not edit - ]"> */

<'if {![catch {dotgen input notice} notice]} {
  puts [lang c; comment $notice]
}'>
}

# model source files
set model_files {
  fiacre.tcl
}

set compnames [list]
foreach c [dotgen components] {
	lappend compnames [$c name]
}

set outputname [join $compnames -]
set filename [list $outputname ".fcr"]

#foreach c [dotgen components] {
  foreach f $model_files {
    template parse args $corenumb \
	string $header \
        file $f file [join $filename ""]
  }
#}

# template parse args [list $types] \
#     string $header file ../../common/serialize.h file src/serialize.h
# template parse args [list $types] \
#     string $header file ../../common/typecopy.h file src/typecopy.h

# setup build environment
template parse file bootstrap.sh file bootstrap.sh
#template parse args [list $input] file model.Makefile.am file Makefile.am
#template parse file model.configure.ac file configure.ac

file mkdir $odir/autoconf

set deps [list]
foreach d [concat [dotgen input deps] [template deps]] {
  lappend deps "regen: $d"
  lappend deps "$d:"
}
engine mode +overwrite -move-if-change
template parse raw [join $deps "\n"]\n file regen