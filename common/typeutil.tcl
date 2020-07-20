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



proc oprs_service_va_doc { service } {
    if {[llength [$service parameters in inout]] == 0} {
	return ""
    } else {  
	set res ""
	foreach  p [$service parameters in inout] {
	    append res [oprs_type_va_doc  [$p type] $p [$p name]]
	}
	return $res
    }
}


proc oprs_type_va_doc { type param string } {
    switch -glob -- [$type kind] {
	typedef - {* member} {
	    return [oprs_type_va_doc [$type type] $param $string]
	}
	string - array - sequence - "unsigned long" - long - float - double - short - int - char - enum {
	    return " ($string <[$type mangle]>)"
	}
	struct {
	    set res ""
	    foreach e [$type members] {
		append res [oprs_type_va_doc [$e type] $param $string.[$e name]]
	    }
	    return $res
	}
	default {
	    puts stderr "unexpected type: [$type kind]" 
	}
    }
}

proc oprs_service_nva { service } {
    if {[llength [$service parameters in inout]] == 0} {
	return 0
    } else {  
	set res 0
	foreach  p [$service parameters in inout] {
	    set res [expr $res + [oprs_type_nva  [$p type] $p [$p name]]]
	}
	return $res
    }
}


proc oprs_type_nva { type param string } {
    switch -glob -- [$type kind] {
	typedef - {* member} {
	    puts stderr member
	    return [oprs_type_nva [$type type] $param $string]
	}
	string - array - sequence - "unsigned long" - long - float - double - short - int - char - enum {
	 	    return 1
	}
	struct {
	    set res 0
	    foreach e [$type members] {
		set res [expr $res + [oprs_type_nva [$e type] $param $string.[$e name]]]
	    }
	    return $res
	}
	default {
	    puts stderr "unexpected type: [$type kind]" 
	}
    }
}

proc oprs_service_va { service } {
    if {[llength [$service parameters in inout]] == 0} {
	return ""
    } else {  
	set res ""
	foreach  p [$service parameters in inout] {
	    append res [oprs_type_va  [$p type] $p [$p name]]
	}
	return $res
    }
}

proc oprs_type_va { type param string } {
    switch -glob -- [$type kind] {
	typedef - {* member} {
	    puts stderr member
	    return [oprs_type_va [$type type] $param $string]
	}
	string - array - sequence - "unsigned long" - long - float - double - short - int - char - enum {
	    if {[catch {$param initializer $type}]} {
		set mandat 1
	    } else {
		set mandat 0
	    }
	    return ",\"$string\",$mandat,[$type pass reference in->$string],encode_g3_[$type mangle]"
	}
	struct {
	    set res ""
	    foreach e [$type members] {
		append res [oprs_type_va [$e type] $param $string.[$e name]]
	    }
	    return $res
	}
	default {
	    puts stderr "unexpected type: [$type kind]" 
	}
    }
}

# proc oprs_type_glob { type } {
#     set num 0
#     oprs_type $type num
# }

proc oprs_service_in_type_glob { comp service } {
    if {[llength [$service parameters in inout]] == 0} {
	return "nil"
    } else {  
	set num 0
	set res $comp\_[$service name]_input
	foreach  p [$service parameters in inout] {
	    append res " ([$p name] [oprs_type [$p type] num])"
	    incr num
	}
	return "($res)"
    }
}

proc oprs_service_out_type_glob { comp service } {
    if {[llength [$service parameters out inout]] == 0} {
	return "nil"
    } else {  
	set num 0
	set res $comp\_[$service name]_output
	foreach  p [$service parameters out inout] {
	    append res " ([$p name] [oprs_type [$p type] num])"
	    incr num
	}
	return "($res)"
    }
}

proc oprs_port_out_type_glob { comp port } {
    set num 0
    set res "[oprs_type [$port datatype] num]"
    return "($comp\_[$port name] $res)"
}

proc oprs_type { type &num} {
    upvar ${&num} num
    switch -glob -- [$type kind] {
	typedef - {* member} {
	    return [oprs_type [$type type] num]
	}
	enum {
	    return "\$[$type mangle]"
	}
	float - double {
	    return "\$float\_$num"
	}
	string {
	    if {[catch {$type length} l]} {
		return  "\$dynstring\_$num"
	    } else {
		return  "\$string$l\_$num"
	    }
	}
	int {
	    return "\$int\_$num"
	}
	short {
	    return "\$short\_$num"
	}
	char {
	    return "\$char\_$num"
	}
	long {
	    return "\$long\_$num"
	}
	"unsigned long" {
	    return "\$ulong\_$num"
	}
	sequence {
	    if {[catch {$type length} l]} {
		return "\$dynseq\_$num"
	    } else {
		return "\$seq$l\_$num"
	    }
	}
	array {
	    return "\$array[$type length]\_$num"
	}
	struct {
	    set res [$type mangle]
	    foreach e [$type members] {
		append res " ([$e name] [oprs_type [$e type] num])"
		incr num
	    }
	    return "($res)"
	}
	default {
	    puts stderr "unexpected type: [$type kind]" 
	}
    }
}


# --- isfixed --------------------------------------------------------------
#
#
# proc isfixed { type } {
#   set type [$type final]
#   switch -- [$type kind] {
#     struct - union - exception {
#       foreach e [$type members] {
#         if {![isfixed $e]} { return no }
#       }
#       return yes
#     }

#     sequence {
#       if {[catch {$type length}]} { return no }
#       return [isfixed [$type type]]
#     }

#     array {
#       return [isfixed [$type type]]
#     }

#     string {
#       if {[catch {$type length}]} { return no }
#       return yes
#     }

#     default {
#       return yes
#     }
#   }

#   error "reached unreachable"
# }


proc expandtype { type done& } {
  upvar ${done&} done
  set name [$type mangle]
  if {[dict exists $done $name]} return

  switch -- [$type kind] {
    {array} - {sequence} - {typedef} - {struct member} - {union member} {
      expandtype [$type type] done
    }
    {struct} - {union} - {exception} {
      foreach e [$type members] {
        expandtype $e done
      }
    }
  }

  if {[dict exists $done $name]} return
  dict set done $name $type
}

proc thread-safe { component codel } {

  set conflicts [list]
  set input [list]
  set output [list]
  set all [list]
  foreach p [$codel parameters] {
    if {[$p src] eq "local"} continue
    switch -- [$p dir] {
      in { lappend input $p; lappend all $p }
      inout { lappend input $p; lappend output $p; lappend all $p }
      out { lappend output $p; lappend all $p }

      default { error "unsupported direction" }
    }
  }

  if {[catch {$codel task} task]} { set task "" }

  foreach t [$component tasks] {
    if {$t == $task} continue

      foreach c [$t codels] {
        set conflicts {*}[codel-intersect $input $output $c]
	if {[llength $conflicts]} {return 0}
     }
   }
  
  foreach s [$component services] {
    if {[catch {$s task} t]} { set t "" }
    if {$t == $task} continue

    foreach p [$s parameters] {
      if {[$p src] ne "ids"} continue
      switch -- [$p dir] {
        in - inout { set check $all }
        out { set check $output }
      }

      foreach q $check {
        if {[parameter-match $p $q]} {
           return 0
        }
      }
    }

    foreach c [$s validate] {
      set conflicts {*}[codel-intersect $input $output $c]
      if {[llength $conflicts]} {return 0}
    }

    if {![catch {$codel service} service]} {
      if {$service in [$s interrupts]} continue
    }

    foreach c [list {*}[$s validate] {*}[$s codels]] {
      set conflicts {*}[codel-intersect $input $output $c]
      if {[llength $conflicts]} {return 0}
    }
  }

  return 1
}


proc codel-intersect { input output codel } {
  set output2 [list]
  set all2 [list]
  foreach p [$codel parameters] {
    if {[$p src] eq "local"} continue
    switch -- [$p dir] {
      in { lappend all2 $p }
      inout - out { lappend output2 $p; lappend all2 $p }

      default { error "unsupported direction" }
    }
  }
  foreach p [$codel parameters out inout] {
    if {[$p src] ne "local"} { lappend output2 $p }
  }

  foreach p $input {
    foreach q $output2 {
      if {[parameter-match $p $q]} { return $codel }
    }
  }
  foreach p $output {
    foreach q $all2 {
      if {[parameter-match $p $q]} { return $codel }
    }
  }

  return [list]
}

proc parameter-match { p q } {
  if {[$p base] ne [$q base]} { return no }
  set pm [$p member]
  set qm [$q member]

  if {$pm eq ""} { return yes }
  if {$qm eq ""} { return yes }
  if {[string first $pm $qm] == 0} { return yes }
  if {[string first $qm $pm] == 0} { return yes }
  return no
}

