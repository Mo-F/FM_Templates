#
# Copyright (c) 2012-2013 LAAS/CNRS
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
#                                           Anthony Mallet on Mon Feb 27 2012
#





proc mutex-indexes { component codel } {

  set index 0
  set indexes [list]
  

  foreach t [$component tasks] {
		
      foreach c [$t codels] {
		 
		if {$c == $codel} {set myindex $index
		incr index
		
		} else {
		  if {[llength [$c mutex]]} {
		  if {$codel in [$c mutex]} {
				lappend indexes [list $index [$c name] [$t name]]}
				incr index}}
		 
	}}

  foreach s [$component services] {	
  
	foreach c [$s validate] {
		
		if {$codel == $c} {
				
				set myindex $index
				incr index
		
				} else {
					
				if {[llength [$c mutex]]} {
					if {$codel in [$c mutex]} {
					lappend indexes [list $index [join [list "validate" [$c name]] _] [$s name]]}
					incr index}}
			}
		
	
	if {[$s kind]!="activity"} {
		
	if {$codel == $s || $codel in [$s codels]} {
		
		set myindex $index
		incr index
		
		} else {
			
		set conflicts [$s mutex]
		foreach c [$s codels] {
			foreach m [$c mutex] {lappend conflicts $m}}
			
		if {[llength $conflicts]} {	
		
		if {$codel in $conflicts} {
			lappend indexes [list $index [$s kind] [$s name]]}
			incr index}}
			
		
	} else {
		
		foreach c [$s codels] {
			
			if {$codel == $c} {
				
				set myindex $index
				incr index
		
				} else {
					
				if {[llength [$c mutex]]} {
		
				if {$codel in [$c mutex]} {
				lappend indexes [list $index [$c name] [$s name]]}
				incr index
				}}
			}
	}
} 

  set Indexes [linsert $indexes end $myindex] 
  return $Indexes

}



proc mutex-ports { dotgen codel } {
  set sites [list]
  foreach p [$codel parameters] {
	  if {![catch {$p port}]} {
	  set counter -1
	  set test 0
	  foreach c [dotgen components] {
		  foreach po [$c ports] {
			  if {[$po name]=="genom_state"} {continue}
			  if {[$po dir]!="in"} {
			  incr counter
			  if {[$po name] == [$p name]} {
				  lappend sites [list $counter [$p dir] [$p name] [$c name]]
				  set test 1
				  break}}}
			  if {$test} {break}}}} 

	return $sites
						
							
					
}

proc ports-number { dotgen } {
	set counter 0
	foreach c [dotgen components] {
		  foreach p [$c ports] { 
			  if {[$p name]=="genom_state" || [$p dir]=="in"} {continue}
			  incr counter}}
	return $counter
}



