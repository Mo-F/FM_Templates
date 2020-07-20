<'
# Copyright (c) 2012-2015 LAAS/CNRS
# All rights reserved.
#
# Redistribution and use  in source  and binary  forms,  with or without
# modification, are permitted provided that the following conditions are
# met:
#
#   1. Redistributions of  source  code must retain the  above copyright
#      notice and this list of conditions.
#   2. Redistributions in binary form must reproduce the above copyright
#      notice and  this list of  conditions in the  documentation and/or
#      other materials provided with the distribution.
#
#                                      Felix Ingrand on July 20 2012
#


#if {[llength $argv] != 1} { error "expected arguments: component" }
#lassign $argv component

#set types [$component types public]

# compute handy shortcuts
#set comp [$component name]
#set COMP [string toupper [$component name]]

#lang c
'>

<'#if {[llength $argv] != 1} { error "expected arguments: component" }
#lassign $argv component

#set types [$component types public]

# compute handy shortcuts
#set comp [$component name]
#set COMP [string toupper [$component name]]

lang c
'>

/* This module is automatically generated. Services codels' temporization is set according to their respective WCET
										     Mohammed Foughali (LAAS, November 2015) */
/* constants & types */<'
if {[llength [dotgen components]]>1} {
set portsnumber [ports-number dotgen]'>
	
		/* Global constants & types: ports mutual exclusion */
const PortsNumber: nat is <"$portsnumber">
type PortsArray is array <"$portsnumber"> of bool<"\n\n"><'}
if {$argv} {'><"\n">const CoresNumber: 0..<"$argv"> is <"$argv"> /* number of available processors */<'}



foreach comp [dotgen components] {'><"\n\n"><'
set numbservpertask [list]
set max 2
set codelnumber 0
set lengthinterr [list]
set selfincompatible [list]
set services [list]
set numbincompertask [list]
set activities [list]
set functions [list]
set attributes [list]
set needsvalidate [list]
foreach t [$comp tasks] {
foreach c [$t codels] {
if {[llength [$c mutex]] } {
	incr codelnumber}}
set temp 0
set numberincomp 0
foreach s [$t services] {
incr temp
lappend activities $s
foreach i [$s interrupts] {
if {[$i name]==[$s name]} {
incr numberincomp
lappend selfincompatible $s
break}}}
lappend numbservpertask $temp
lappend numbincompertask $numberincomp}

foreach s [$comp services] {
if {[$s name]!="abort_activity" && [$s name]!="kill" && [$s name]!="connect_port" && [$s name]!="connect_service"} {
	
lappend lengthinterr [llength [$s interrupts]]
foreach c [$s validate] {
	if {[llength [$c mutex]] } {
	incr codelnumber}
lappend needsvalidate $s}

if {[$s kind]=="activity"} {

foreach c [$s codels] {
	if {[llength [$c mutex]] } {
	incr codelnumber}}

} else {

	
if {[llength [$s mutex]] } {
	incr codelnumber

} else {
	
	foreach c [$s codels] {
		 if {[llength [$c mutex]] } {
		 incr codelnumber
		 break}

}}
if {[$s kind]=="function"} {lappend functions $s} 
if {[$s kind]=="attribute"} {lappend attributes $s}

}
}
}
set services [concat $activities $functions $attributes]
set maxint 0
if {[llength $lengthinterr]} {
set maxint [lindex $lengthinterr 0]
foreach l $lengthinterr {
if {$l > $maxint} {
set maxint $l}}}

if {$codelnumber} {'>
const CodNumber_<"[$comp name]">: nat is <"$codelnumber"> /* number of codels: determines the size of the mutex boolean array */<"\n"><'}
if {[llength $activities]} {'>
const Max_<"[$comp name]">: nat is <"$max"> /* maximum of instances allowed per one self-compatible service, freely changeable */
<'set index -1
set widths [list]
set indexes [list]
foreach t [$comp tasks] {
incr index'>
const Nbactivities_<"[$t name]">_<"[$comp name]">: nat is <"[lindex $numbservpertask $index]">/* number of services for <"[$t name]"> task*/
const SelfIncomp_<"[$t name]">_<"[$comp name]">: 0..Nbactivities_<"[$t name]">_<"[$comp name]"> is <"[lindex $numbincompertask $index]">/* number of self-incompatible services */<'
set width [expr [lindex $numbincompertask $index]*2+$max* [expr [lindex $numbservpertask $index]-[lindex $numbincompertask $index]]]
lappend widths $width
}
set index 0
foreach w $widths {
if {$index==0} {
lappend indexes $index
} else {
lappend indexes [expr [lindex $indexes [expr $index-1]]+[lindex $widths [expr $index-1]]]}
incr index}'><"\n">const maxinterrupts_<"[$comp name]">: nat is <"$maxint"> /* maximum length of the interrupts queue */<'}
if {[llength $services]} {'>

const Nbnonact_<"[$comp name]">: nat is <"[expr [llength $functions] + [llength $attributes]]"> /* number of attributes and functions */<'}
if {[llength $activities]} {
foreach t [$comp tasks] {'>
<"\n">const width_<"[$t name]">_<"[$comp name]">: nat is SelfIncomp_<"[$t name]">_<"[$comp name]">*2+(Nbactivities_<"[$t name]">_<"[$comp name]">-SelfIncomp_<"[$t name]">_<"[$comp name]">)*Max_<"[$comp name]"><'}'><"\n">const width_<"[$comp name]">: nat is <'set index 1
foreach t [$comp tasks] {'>width_<"[$t name]">_<"[$comp name]"><'
if {$t!=[lindex [$comp tasks] end]} {'>+<'}}'>/* size of the activities part*/<'}
if {[llength $services]} {
if {[llength $activities]} {'><"\n">const size_<"[$comp name]">: nat is width_<"[$comp name]">+1<'
} else {'><"\n">const size_<"[$comp name]">: nat is 1<'}}'><"\n">type service_<"[$comp name]"> is union <'
foreach s $services {'>
<"[$s name]"><'
if {[$s kind]=="activity"} {'>_<"[[$s task] name]"><'}'>_<"[$comp name]"> | <'}'> None_<"[$comp name]"> end /* possible names of services */
<"\n">type status_<"[$comp name]"> is union  VOID_<"[$comp name]"> /* inactive */ |START_<"[$comp name]"> /*waiting*/ | RUNNING_<"[$comp name]"> /*active*/ | STOP_<"[$comp name]"> /*being interrupted*/ | ETHER_<"[$comp name]"> end<"\n">type dynamicservice_<"[$comp name]"> is record name: service_<"[$comp name]">, current: status_<"[$comp name]">, instance: 0..<'
if {[llength $activities]} {'>Max_<"[$comp name]"><'
} else {'>0<'}'> end /* type of the "turn" shared variables */<'
if {[llength $services]} {'>

type staticScheduler_<"[$comp name]"> is array size_<"[$comp name]"> of dynamicservice_<"[$comp name]"> /* type of the execution array */<'}
if {[llength $activities]} {'>

type Interrupts_<"[$comp name]"> is queue maxinterrupts_<"[$comp name]"> of service_<"[$comp name]"> /* field "interrupts" of a GenoM service */
type IndexA_<"[$comp name]"> is 0..width_<"[$comp name]">-1<'}
if {[llength $services]} {'>

type Index_<"[$comp name]"> is 0..size_<"[$comp name]">-1<'}'><"\n">type CRName_<"[$comp name]"> is union Kill_<"[$comp name]"><'
if {[llength $activities]} {'> | Abort_<"[$comp name]"><'}'> end
type CR_<"[$comp name]"> is record name: CRName_<"[$comp name]">, who: <'
if {[llength $activities]} {'>IndexA_<"[$comp name]"><'
} else {'>0..0<'}'> end
<'if {[llength $activities]} {
set index 0
foreach t [$comp tasks] {
if {[llength [$t services]]} {'>
const index_<"[$t name]">_<"[$comp name]">: IndexA_<"[$comp name]"> is <"[lindex $indexes $index]"><'
incr index}}}
if {$codelnumber} {'><"\n">type Mutex_<"[$comp name]"> is array CodNumber_<"[$comp name]"> of bool<'}'>


/* functions */<'
if {[llength $activities]} {'>
	
/* can the control task shutdown? */

function endsignal_<"[$comp name]">(execution: staticScheduler_<"[$comp name]">): bool is
var index: Index_<"[$comp name]">
begin
	foreach index do
		if (not (execution[index].current=ETHER_<"[$comp name]">) and not (execution[index].current=VOID_<"[$comp name]">)) then return false end
	end; return true
end


/* is an execution task runnable? */
function running_<"[$comp name]"> (execution: staticScheduler_<"[$comp name]">, index: Index_<"[$comp name]">, widthtask: nat): bool is
var limit: nat
begin
	limit:= index+widthtask;
	while index<limit do
		if not (execution[index].current=ETHER_<"[$comp name]"> or execution[index].current=VOID_<"[$comp name]">) then return true end;
		index:= index+1
	end; return false
end


/* services that I interrupt */
function Iinterrupt_<"[$comp name]"> (serv: service_<"[$comp name]">): Interrupts_<"[$comp name]"> is
begin
	case serv of<'set first 0
if {[llength $services]} {
foreach s $services {
if {[llength [$s interrupts]]} {'><"\n\t"><'
if {$first} {'>| <'}'>
<"[$s name]"><'if {[$s kind]=="activity"} {'>_<"[[$s task] name]"><'}'>_<"[$comp name]"> -> return {|<'
set first 1
if {[llength [$s interrupts]]==[llength [$comp services]]} {
foreach a $activities {'>
<"[$a name]">_<"[[$a task] name]">_<"[$comp name]"><'if {$a != [lindex $activities end]} {'>,<'}}
} else {
foreach i [$s interrupts] {'>
<"[$i name]">_<"[[$i task] name]">_<"[$comp name]"><'if {$i != [lindex [$s interrupts] end]} {'>,<'}}}'>|} <'}}'><"\n\t"><'
if {$first} {'>| <'}}'> any -> return {||}
	end
end

/* check for incompatibility */
function incomp_<"[$comp name]"> (interrupts: Interrupts_<"[$comp name]">, serv: service_<"[$comp name]">) : bool is
begin
    if empty (interrupts) then return false end;
    if first (interrupts) = serv then return true end;
    return incomp_<"[$comp name]"> (dequeue (interrupts), serv)
end

/* returns indexes of incompatible services */
function sitesincomp_<"[$comp name]"> (serv: service_<"[$comp name]">, execution: staticScheduler_<"[$comp name]">, indexx: Index_<"[$comp name]">): queue width_<"[$comp name]"> of IndexA_<"[$comp name]"> is
var index: IndexA_<"[$comp name]">:=0, sites: queue width_<"[$comp name]"> of IndexA_<"[$comp name]">:= {||}
begin
	foreach index do
		if not (index=indexx) then
			if incomp_<"[$comp name]">(Iinterrupt_<"[$comp name]">(serv), execution[index].name) then
				sites:= enqueue(sites, index)
			end
		end
	end; return sites
end

/* manage interrupts */
function manageinterrupt_<"[$comp name]"> (execution: staticScheduler_<"[$comp name]">, index: Index_<"[$comp name]">): status_<"[$comp name]"> is
begin
	if execution[index].current= START_<"[$comp name]"> then return ETHER_<"[$comp name]"> end;
	if execution[index].current= RUNNING_<"[$comp name]"> then return STOP_<"[$comp name]"> end;
	return execution[index].current
end

/* clear */
function clear_<"[$comp name]"> (execution: staticScheduler_<"[$comp name]">, index: Index_<"[$comp name]">): status_<"[$comp name]"> is
begin
	if execution[index].current=ETHER_<"[$comp name]"> then return VOID_<"[$comp name]"> end;
	return execution[index].current
end

/* unblock the services line */
function next_<"[$comp name]"> (execution: staticScheduler_<"[$comp name]">, index: Index_<"[$comp name]">, indextask: IndexA_<"[$comp name]">, widthtask: nat): Index_<"[$comp name]"> is
var limit: nat
begin
	limit:= indextask+widthtask;
	while index<limit do 
		if (execution[index].current=RUNNING_<"[$comp name]"> or execution[index].current=STOP_<"[$comp name]">) then 
		   return index
		end;
		index:= index+1 
	end; return $(size_<"[$comp name]">-1)
end<'}
if {[llength $services]} {'>

/* look for a free slot. Optimized: we no longer check all the array elements */
function multipleinstances_<"[$comp name]"> (execution: staticScheduler_<"[$comp name]">, temp: service_<"[$comp name]">) : Index_<"[$comp name]"> is
var index: Index_<"[$comp name]"> := 0
begin
	foreach index do 
		if execution[index].name=temp then 
			while (index<width_<"[$comp name]"> and execution[index].name=temp) do 
				if execution[index].current=VOID_<"[$comp name]"> then return index end;
				index:=index+1
			end;
			return $(size_<"[$comp name]">-1)
		end
	end;
	return $(size_<"[$comp name]">-1) /* never reached, but the compiler complains if omitted */
end<'}'>


/* Processes */

<' if {[llength [dotgen components]]==1} {'>

/* client */

process client_<"[$comp name]">[<'
if {[llength $services]} {'>req_<"[$comp name]">: out service_<"[$comp name]">, <'}'>Creq_<"[$comp name]">: out CR_<"[$comp name]"><'
if {[llength $activities]} {'>, reqimm_<"[$comp name]">: out service_<"[$comp name]">, Creqimm_<"[$comp name]">: out CR_<"[$comp name]"><'}'>] is
states start<'
if {[llength $activities]} {'>

var c: IndexA_<"[$comp name]"><'}'>

from start <'if {[llength $services]} {'><"\n\t">select <'
foreach s $services {'><"\n\t"><'
if {$s != [lindex $services 0]} {'>[] <'}'>req_<"[$comp name]">!<"[$s name]"><'
if {[$s kind]=="activity"} {'>_<"[[$s task] name]"><'}'>_<"[$comp name]"><'}
if {[llength $activities]} {
foreach s $services {'><"\n\t">[] reqimm_<"[$comp name]">!<"[$s name]"><'
if {[$s kind]=="activity"} {'>_<"[[$s task] name]"><'}'>_<"[$comp name]"><'}'><"\n\t">[] reqimm_<"[$comp name]">!None_<"[$comp name]"><'}'><"\n\t">[] Creq_<"[$comp name]">!{name=Kill_<"[$comp name]">, who=0}<'
if {[llength $activities]} {'><"\n\t">[] c:= any; Creq_<"[$comp name]">!{name=Abort_<"[$comp name]">, who=c}
	[] Creqimm_<"[$comp name]">!{name=Kill_<"[$comp name]">, who=0}
	[] c:= any; Creqimm_<"[$comp name]">!{name=Abort_<"[$comp name]">, who=c}<'}'><"\n\t">end; to start<'
} else {'>Creq_<"[$comp name]">!{name=Kill_<"[$comp name]">, who=0}; to start<'}}'>

/* control task */

<'set lastperm 0
for {set k [llength [$comp tasks]]} {$k > 0} {incr k -1} {
	if {[llength [[lindex [$comp tasks] [expr $k-1]] codels]]} {set lastperm $k
		break}}'>

process CT_<"[$comp name]">[<'
if {$lastperm} {'>end_spawn_<"[[lindex [$comp tasks] [expr $k-1]] name]">_<"[$comp name]">: sync, <'}
if {[llength $services]} {'>req_<"[$comp name]">: in service_<"[$comp name]">, <'}'>Creq_<"[$comp name]">: in CR_<"[$comp name]"><'
if {[llength $activities]} {'>, reqimm_<"[$comp name]">: in service_<"[$comp name]">, Creqimm_<"[$comp name]">: in CR_<"[$comp name]"><'}'>] (<'
if {$argv} {'>&Cores: 0..CoresNumber, <'}
if {[llength $services]} {'>&execution_<"[$comp name]">: staticScheduler_<"[$comp name]">, &sched_<"[$comp name]">: bool, <'}'>&shut_<"[$comp name]">: bool<'
if {$codelnumber} {'>, &DMutex_<"[$comp name]">: Mutex_<"[$comp name]"><'}
if {[llength [dotgen components]]>1} {'>, &Ports: PortsArray<'}'>) is
states <'
if {$lastperm} {'>unspawned, <'}'>start,<'

if {[llength $activities]} {'> wait_, manage, finish, <'}
foreach n $needsvalidate {'>v_<"[$n name]">_<'
if {[$n kind] == "activity"} {'><"[[$n task] name]"><'}'>, <'
if {[llength [[$n validate] mutex]]} {'>v_<"[$n name]"><'
if {[$n kind] == "activity"} {'>_<"[[$n task] name]"><'}'>_2, <'}}

foreach a $attributes {
set conflicts [$a mutex]
foreach c [$a codels] {
	lappend conflicts [$c mutex]}'><"[$a name]">_, <'
if {[llength $conflicts]} {'><"[$a name]">_2, <'}}

foreach f $functions {
set conflicts [$f mutex]
foreach c [$f codels] {
	lappend conflicts [$c mutex]}'><"[$f name]">_, <'
if {[llength $conflicts]} {'><"[$f name]">_2, <'}}'>shutdown
var <'
if {[llength $services]} {'>index: Index_<"[$comp name]">, temp: service_<"[$comp name]">, <'}
if {[llength $activities]} {'>index2: IndexA_<"[$comp name]">, line: queue width_<"[$comp name]"> of 0..size_<"[$comp name]">-1, launcher: bool:= true, <'}'>tempCR: CR_<"[$comp name]">

<'if {$lastperm} {'>
	
from unspawned
end_spawn_<"[[lindex [$comp tasks] [expr $lastperm-1]] name]">_<"[$comp name]">;
to start 

<'}'>

from start <'
if {$argv} {'>on Cores>0; Cores:= Cores-1; <'}
if {[llength $services]} {'> index:=0;<'}'><"\n"><'
if {[llength $activities]} {'>
	if sched_<"[$comp name]"> then sched_<"[$comp name]">:= false; 
	select	reqimm_<"[$comp name]">?temp; if temp=None_<"[$comp name]"> then to finish else<"\n\t\t\t">case temp of <"\n\t\t\t"><'

foreach se $services {
if {[$se kind] != "activity"} {'><"[$se name]">_<"[$comp name]"> -> to <'
if {$se in $needsvalidate} {'>v_<'}'><"[$se name]">_<"\n\t\t\t">| <'
} else {
if {$se in $needsvalidate} {'><"[$se name]">_<"[[$se task] name]">_<"[$comp name]"> -> to v_<"[$se name]">_<"[[$se task] name]"><"\n\t\t\t">| <'}}
}'> any -> to manage<"\n\t\t\t">end<"\n\t\t">end	     
	     [] Creqimm_<"[$comp name]">?tempCR; <"\n\t">if tempCR.name=Kill_<"[$comp name]"> then
		foreach index2 do
			execution_<"[$comp name]">[index2].current:= manageinterrupt_<"[$comp name]"> (execution_<"[$comp name]">, index2)
		end; shut_<"[$comp name]">:= true; <'
		if {$argv} {'>Cores:= Cores+1; <'}'> to wait_
	else  execution_<"[$comp name]">[tempCR.who].current:= manageinterrupt_<"[$comp name]"> (execution_<"[$comp name]">, tempCR.who); to finish
		end
	end

 	else<'}'><"\n"><'
if {[llength $services]} {'>
	select	req_<"[$comp name]">?temp; <"\n\t\t\t">case temp of <"\n\t\t\t"><'

foreach se $services {
if {[$se kind] != "activity"} {'><"[$se name]">_<"[$comp name]"> -> to <'
if {$se in $needsvalidate} {'>v_<'}'><"[$se name]">_<"\n\t\t\t">| <'
} else {
if {$se in $needsvalidate} {'><"[$se name]">_<"[[$se task] name]">_<"[$comp name]"> -> to v_<"[$se name]">_<"[[$se task] name]"><"\n\t\t\t">| <'}}
}'> any -> to manage<"\n\t\t\t">end
	     [] Creq_<"[$comp name]">?tempCR;

<'
	     if {[llength $activities]} {'><"\n\t">if tempCR.name=Kill_<"[$comp name]"> then
		foreach index2 do
			execution_<"[$comp name]">[index2].current:= manageinterrupt_<"[$comp name]"> (execution_<"[$comp name]">, index2)
		end; shut_<"[$comp name]">:= true; <'
		if {$argv} {'>Cores:= Cores+1; <'}'>to wait_
	else  execution_<"[$comp name]">[tempCR.who].current:= manageinterrupt_<"[$comp name]"> (execution_<"[$comp name]">, tempCR.who); to finish
		end
	end<'
} else {'>shut_<"[$comp name]">:= true; to shutdown<'}
} else {'>
	Creq_<"[$comp name]">?tempCR; to shutdown<'}
	
if {[llength $activities]} {'><"\n\t">end<'}
if {[llength $services]} {
foreach n $needsvalidate {'><"\n\n">from v_<"[$n name]">_<'
if {[$n kind] == "activity"} {'><"[[$n task] name]"><'}'><"\n\t">wait <'
if {![llength [[$n validate] mutex]]} {
if {![catch {[$n validate] wcet}]} {'>]0,<"[[[$n validate] wcet] value]">];<'
} else {'>[0,0];<'}'> select
			to <'
if {[$n kind] == "activity"} {'>manage<'
} else {'><"[$n name]">_<'}'>
		     [] to finish
		     end<'
} else {'>[0,0]; on (<'
set mutex [mutex-indexes $comp [$n validate]]
foreach m $mutex {
	if {$m != [lindex $mutex end]} {'>not DMutex_<"[$comp name]">[<"[lindex $m 0]">]/*incmopatible with <"[lindex $m 1]">_<"[lindex $m 2]">*/<'
	if {$m != [lindex $mutex end-1]} {'> and <'}}}'>); DMutex_<"[$comp name]">[<"$m">]:= true; to v_<"[$n name]"><'
if {[$n kind] == "activity"} {'>_<"[[$n task] name]"><'}'>_2<"\n\n">from v_<"[$n name]"><'
if {[$n kind] == "activity"} {'>_<"[[$n task] name]"><'}'>_2<"\n\t">wait <'
if {![catch {[$n validate] wcet}]} {'>]0,<"[[[$n validate] wcet] value]">];<'
} else {'>[0,0];<'}'> DMutex_<"[$comp name]">[<"$m">]:= false;

		     select
			to <'
if {[$n kind] == "activity"} {'>manage<'
} else {'><"[$n name]">_<'}'>
		     [] to finish
		     end<'}}
if {[llength $activities]} {'>		     

from manage 
	wait [0,0];
	index:= multipleinstances_<"[$comp name]">(execution_<"[$comp name]">,temp);
	if index < width_<"[$comp name]"> then	
	execution_<"[$comp name]">[index].current:= START_<"[$comp name]">;
    line:= sitesincomp_<"[$comp name]">(temp, execution_<"[$comp name]">, index);	
	while not (empty line) do
		execution_<"[$comp name]">[first line].current:= manageinterrupt_<"[$comp name]">(execution_<"[$comp name]">, first line);
		line:= dequeue line
	end
	end; to finish
<'}}'><'

foreach a $attributes {'><"\n\n">from <"[$a name]">_<"\n\t">wait <'
set conflicts [$a mutex]
set time 0
foreach c [$a codels] {lappend conflicts [$c mutex]
if {(![catch {$c wcet}]) && ([[$c wcet] value] > $time)} {set time [[$c wcet] value]}}
if {![llength $conflicts]} {'>[0,<"$time">];
line:= sitesincomp_<"[$comp name]">(temp, execution_<"[$comp name]">, $(size_<"[$comp name]">-1));	
	while not (empty line) do
		execution_<"[$comp name]">[first line].current:= manageinterrupt_<"[$comp name]">(execution_<"[$comp name]">, first line);
		line:= dequeue line
	end; to <'
if {[llength $activities]} {'>finish<'
} else {'>start<'}

} else {'>[0,0]; on (<'
set mutex [mutex-indexes $comp $a]
set mutexabd [lreplace $mutex end end]
set mutex2 [list]
	foreach c [$a codels] {
	if {[llength [$c mutex]]} {
	foreach m [mutex-indexes $comp $c] {
		if {$m != [lindex [mutex-indexes $comp $c] end]} {
			lappend mutex2 $m}}}}
	set mutexabd [concat $mutexabd $mutex2]
	set mutexabd [lsort -unique $mutexabd]
	set mutex [concat $mutexabd [lindex $mutex end]]
foreach m $mutex {
	if {$m != [lindex $mutex end]} {'>not DMutex_<"[$comp name]">[<"[lindex $m 0]">]/*incmopatible with <"[lindex $m 1]">_<"[lindex $m 2]">*/<'
	if {$m != [lindex $mutex end-1]} {'> and <'}}}'>); DMutex_<"[$comp name]">[<"$m">]:= true;<"\n\t"> to <"[$a name]">_2<"\n\n">from <"[$a name]">_2
<"\n\t">wait [0,<"$time">]; DMutex_<"[$comp name]">[<"$m">]:= false; 
line:= sitesincomp_<"[$comp name]">(temp, execution_<"[$comp name]">, $(size_<"[$comp name]">-1));	
	while not (empty line) do
		execution_<"[$comp name]">[first line].current:= manageinterrupt_<"[$comp name]">(execution_<"[$comp name]">, first line);
		line:= dequeue line
	end; to <'
if {[llength $activities]} {'>finish<'
} else {'>start<'}}}


foreach f $functions {'><"\n\n">from <"[$f name]">_<"\n\t">wait <'
set conflicts [$f mutex]
set time 0
foreach c [$f codels] {lappend conflicts [$c mutex]
if {(![catch {$c wcet}]) && ([[$c wcet] value] > $time)} {set time [[$c wcet] value]}}
if {![llength $conflicts]} {'>[0,<"$time">]; 

line:= sitesincomp_<"[$comp name]">(temp, execution_<"[$comp name]">, $(size_<"[$comp name]">-1));	
	while not (empty line) do
		execution_<"[$comp name]">[first line].current:= manageinterrupt_<"[$comp name]">(execution_<"[$comp name]">, first line);
		line:= dequeue line
	end; to <'
if {[llength $activities]} {'>finish<'
} else {'>start<'}

} else {'>[0,0]; on (<'
set mutex [mutex-indexes $comp $f]
set mutexabd [lreplace $mutex end end]
set mutex2 [list]
	foreach c [$f codels] {
	if {[llength [$c mutex]]} {
	foreach m [mutex-indexes $comp $c] {
		if {$m != [lindex [mutex-indexes $comp $c] end]} {
			lappend mutex2 $m}}}}
	set mutexabd [concat $mutexabd $mutex2]
	set mutexabd [lsort -unique $mutexabd]
	set mutex [concat $mutexabd [lindex $mutex end]]
foreach m $mutex {
	if {$m != [lindex $mutex end]} {'>not DMutex_<"[$comp name]">[<"[lindex $m 0]">]/*incmopatible with <"[lindex $m 1]">_<"[lindex $m 2]">*/<'
	if {$m != [lindex $mutex end-1]} {'> and <'}}}'>); DMutex_<"[$comp name]">[<"$m">]:= true;<"\n\t"> to <"[$f name]">_2<"\n\n">from <"[$f name]">_2
<"\n\t">wait [0,<"$time">]; DMutex_<"[$comp name]">[<"$m">]:= false; 
line:= sitesincomp_<"[$comp name]">(temp, execution_<"[$comp name]">, $(size_<"[$comp name]">-1));	
	while not (empty line) do
		execution_<"[$comp name]">[first line].current:= manageinterrupt_<"[$comp name]">(execution_<"[$comp name]">, first line);
		line:= dequeue line
	end; to <'
if {[llength $activities]} {'>finish<'
} else {'>start<'}
}}

if {[llength $activities]} {'>

 
from finish 
	wait [0,0];
	
	/* update final replies */
	foreach index2 do
		execution_<"[$comp name]">[index2].current:= clear_<"[$comp name]"> (execution_<"[$comp name]">, index2)
	end;	
	/* process pending activities */
	launcher:= true;
	foreach index2 do
		if execution_<"[$comp name]">[index2].current=START_<"[$comp name]"> then
			line:= sitesincomp_<"[$comp name]">(execution_<"[$comp name]">[index2].name, execution_<"[$comp name]">, index2);
			while not (empty line) do
				if (execution_<"[$comp name]">[first line].current=STOP_<"[$comp name]"> or execution_<"[$comp name]">[first line].current=RUNNING_<"[$comp name]">) then launcher:= false end; 
			line:= dequeue line
			end;
			if launcher then execution_<"[$comp name]">[index2].current:= RUNNING_<"[$comp name]"> end;
			launcher:= true
		end
	end;	 
	
	<'
	if {$argv} {'>Cores:= Cores+1; <'}'> to start
	
from wait_ 
	on endsignal_<"[$comp name]">(execution_<"[$comp name]">);
	wait [0,0];
	foreach index2 do
		execution_<"[$comp name]">[index2].current:= clear_<"[$comp name]"> (execution_<"[$comp name]">, index2)
	end;
	to shutdown

<'}



set last "noperm"

foreach t [$comp tasks] {
if {![catch {$t period}]} {'><"\n">
/* <"[$t name]"> timer */
 
process timer_<"[$t name]">_<"[$comp name]"> [shuttimer_<"[$t name]">_<"[$comp name]">: sync<'
if {$last != "noperm"} {'>, end_spawn_<"$last">_<"[$comp name]">: sync<'}'>](&tick_<"[$t name]">_<"[$comp name]">: bool) is
		
states <'
if {$last != "noperm"} {'>idle, <'}'> start, shutdown<'

if {$last != "noperm"} {'>


from idle

end_spawn_<"$last">_<"[$comp name]">; to start

<'}'>

from start
	select 
	wait[<"[[$t period] value]">, <"[[$t period] value]">]; tick_<"[$t name]">_<"[$comp name]">:= true; to start
	[] shuttimer_<"[$t name]">_<"[$comp name]">; to shutdown
	end

<'}'>

/* <"[$t name]"> manager */

process Taskmanager_<"[$t name]">_<"[$comp name]"> <'
if {![catch {$t period}]} {'>[shuttimer_<"[$t name]">_<"[$comp name]">: sync]<'}'> (&turn_<"[$t name]">_<"[$comp name]"> :dynamicservice_<"[$comp name]"><'
if {[llength [$t services]]} {'>, &execution_<"[$comp name]"> : staticScheduler_<"[$comp name]">, &finished_<"[$t name]">_<"[$comp name]">: bool, &sched_<"[$comp name]">: bool<'}
if {$argv} {'>, &Cores: 0..CoresNumber<'}
if {![catch {$t period}]} {'>, &tick_<"[$t name]">_<"[$comp name]">: bool<'}
if {[llength [$t codels]]} {'>, &endperm_<"[$t name]">_<"[$comp name]">: bool<'}'>, &lock_<"[$t name]">_<"[$comp name]">:0..1, &shut_<"[$comp name]">: bool) is
states start, orchestrate, shutdown

<'if {[llength [$t services]]} {'>var index: Index_<"[$comp name]">, firstfiring: bool, temp: bool:= false<'}'>
  
from start<"\n\t"><' 
if {![catch {$t period}]} {'><"\n\t"> on tick_<"[$t name]">_<"[$comp name]">; tick_<"[$t name]">_<"[$comp name]">:= false; <'}
if {[llength [$t services]]} {'>index:=index_<"[$t name]">_<"[$comp name]">; firstfiring:= true;<'}'>
<"\n\t">if (shut_<"[$comp name]"><'
if {[llength [$t services]]} {'> and not (running_<"[$comp name]">(execution_<"[$comp name]">, index_<"[$t name]">_<"[$comp name]">, width_<"[$t name]">_<"[$comp name]">))<'}'>) then <'
if {![catch {$t period}]} {'> shuttimer_<"[$t name]">_<"[$comp name]">;<'}'> to shutdown 

	else wait[0,0];<'
if {$argv} {'>on Cores>0; Cores:= Cores-1; <'}
if {[llength [$t codels]]} {'><"\n\t\t">if not endperm_<"[$t name]">_<"[$comp name]"> then lock_<"[$t name]">_<"[$comp name]">:= 1 end; to orchestrate<'
} else {'><"\n\t\t">to orchestrate<'}'><"\n\t">end

from orchestrate
	wait [0,0]; on lock_<"[$t name]">_<"[$comp name]">=0;<' 
	if {![llength [$t services]]} {
if {$argv} {'>Cores:= Cores+1; <'}'> to start<'
	} else {'>
		
	if not firstfiring then
		if finished_<"[$t name]">_<"[$comp name]"> then
			execution_<"[$comp name]">[index].current:=ETHER_<"[$comp name]">;
			finished_<"[$t name]">_<"[$comp name]">:= false;
			if not temp then temp:= true end
		end;
		index:= next_<"[$comp name]">(execution_<"[$comp name]">, index+1, index_<"[$t name]">_<"[$comp name]">, width_<"[$t name]">_<"[$comp name]">);
		turn_<"[$t name]">_<"[$comp name]">:= execution_<"[$comp name]">[index];
		if index=width_<"[$comp name]"> then
		if temp then temp:= false; sched_<"[$comp name]">:= true end; <'
		if {$argv} {'>Cores:= Cores+1; <'}'>to start
		else
		lock_<"[$t name]">_<"[$comp name]">:= 1; to orchestrate
		end
	end;
	firstfiring:= false;
	index:= next_<"[$comp name]">(execution_<"[$comp name]">, index, index_<"[$t name]">_<"[$comp name]">, width_<"[$t name]">_<"[$comp name]">);
	if index=width_<"[$comp name]"> then <'
if {$argv} {'>Cores:= Cores+1; <'}'>to start end;
	turn_<"[$t name]">_<"[$comp name]">:= execution_<"[$comp name]">[index];
	lock_<"[$t name]">_<"[$comp name]">:= 1; to orchestrate<'}
	
if {[llength [$t codels]]} {'><"\n\n">/* <"[$t name]"> permanent activity automaton */

process Task_<"[$t name]">_<"[$comp name]"> [end_spawn_<"[$t name]">_<"[$comp name]">: sync<'
if {$last!="noperm"} {'>, end_spawn_<"$last">_<"[$comp name]">: sync<'}'>] (&lock_<"[$t name]">_<"[$comp name]">: 0..1, &endperm_<"[$t name]">_<"[$comp name]">: bool, &turn_<"[$t name]">_<"[$comp name]">: dynamicservice_<"[$comp name]"><'
if {$codelnumber} {'>, &DMutex_<"[$comp name]">: Mutex_<"[$comp name]"><'}
if {[llength [dotgen components]]>1} {'>, &Ports: PortsArray<'}'>) is
states <'
if {$last != "noperm"} {'>idle, <'}
set stop 0
foreach c [$t codels] {
set ports [list]
if {[llength [dotgen components]]>1} {
	set ports [mutex-ports dotgen $c]}
foreach tr [$c triggers] {
if {!$stop && [$tr name]=="stop"} {set stop 1}
if {![llength [$c mutex]]  && ![llength $ports]} {'><"[$tr name]">_, <'
} else {'><"[$tr name]">_, <"[$tr name]">_2, <'}}}'>ether_

var first_exec: bool:= true

<'if {$last != "noperm"} {'>
	
from idle
	end_spawn_<"$last">_<"[$comp name]">; to start_
	
<'}

foreach c [$t codels] {
foreach tr [$c triggers] {'><"\n\n">from <'
set ports [list]
if {[llength [dotgen components]]>1} {
	set ports [mutex-ports dotgen $c]}
if {![llength [$c mutex]]  && ![llength $ports]} {'><"[$tr name]">_/* thread-safe codel */<"\n\t"><'
if {[$tr name] == "start"} {'><"\n\t">if first_exec then end_spawn_<"[$t name]">_<"[$comp name]">
	else on (turn_<"[$t name]">_<"[$comp name]">.name= None_<"[$comp name]"> and lock_<"[$t name]">_<"[$comp name]">=1);
	wait <'
if {![catch {$c wcet}]} {'>]0,<"[[$c wcet] value]">] <'
} else {'>[0,0] <'}'>

	end;


<'} else {'><"\n\t">if not first_exec then on (turn_<"[$t name]">_<"[$comp name]">.name= None_<"[$comp name]"> and lock_<"[$t name]">_<"[$comp name]">=1) end;
	wait <'
if {![catch {$c wcet}]} {'>]0,<"[[$c wcet] value]">]; <'
} else {'>[0,0]; <'}}
if {[llength [$c yields]]==1} {
if {[[$c yields] name]=="ether"} {'>endperm_<"[$t name]">_<"[$comp name]">:=true; lock_<"[$t name]">_<"[$comp name]">:=0; to ether_<'
} else {
if {[[$c yields] kind] == "pause event"} {'>
lock_<"[$t name]">_<"[$comp name]">:=0; first_exec:= false; <'}'>to <"[[$c yields] name]">_<'}
} else {'><"\n\t">select<'
foreach y [$c yields] {'><"\n\t"><'
if {$y!=[lindex [$c yields] 0]} {'>[] <'}
if {[$y name]=="ether"} {'>endperm_<"[$t name]">_<"[$comp name]">:=true; lock_<"[$t name]">_<"[$comp name]">:=0; to ether_<'
} else {
if {[$y kind] == "pause event"} {'>
lock_<"[$t name]">_<"[$comp name]">:=0; first_exec:= false; <'}'>to <"[$y name]">_<'
}}'><"\n\t">end<'}

} else {'><"[$tr name]">_<"\n\t">wait [0,0]; 
  
on (<'
set mutex [mutex-indexes $comp $c]
foreach m $mutex {
	if {$m != [lindex $mutex end]} {'>not DMutex_<"[$comp name]">[<"[lindex $m 0]">]/*incmopatible with <"[lindex $m 1]">_<"[lindex $m 2]">*/<'
	if {$m != [lindex $mutex end-1]} {'> and <'}}}
	if {[llength [$c mutex]] && [llength $ports]} {'> and <'}
foreach p $ports {'>not Ports[<"[lindex $p 0]">] /* uses (<"[lindex $p 1]">) the port <"[lindex $p 2]"> of the component <"[lindex $p 3]"> */<'
	if {$p!=[lindex $ports end]} {'> and <'}}'>);
	if not first_exec then on (turn_<"[$t name]">_<"[$comp name]">.name= None_<"[$comp name]"> and lock_<"[$t name]">_<"[$comp name]">=1) end;
	<'if {[llength [$c mutex]]} {'> DMutex_<"[$comp name]">[<"$m">]:= true;<'}
	foreach p $ports {'> Ports[<"[lindex $p 0]">]:= true;<'}'> to <"[$tr name]">_2
	
from <"[$tr name]">_2<'

if {[$tr name] == "start"} {'><"\n\t">if first_exec then end_spawn_<"[$t name]">_<"[$comp name]">
	
	else 
	wait <'
if {![catch {$c wcet}]} {'>]0,<"[[$c wcet] value]">] <'
} else {'>[0,0] <'}'>

	end;
	
<'
} else {'><"\n\t">
	
	wait<'
if {![catch {$c wcet}]} {'>]0,<"[[$c wcet] value]">]; <'
} else {'>[0,0]; <'}}
if {[llength [$c mutex]]} {'>DMutex_<"[$comp name]">[<"$m">]:= false; <'}
	foreach p $ports {'>Ports[<"[lindex $p 0]">]:= false; <'}
if {[llength [$c yields]]==1} {
if {[[$c yields] name]=="ether"} {'>endperm_<"[$t name]">_<"[$comp name]">:=true; lock_<"[$t name]">_<"[$comp name]">:=0; to ether_<'
} else {
if {[[$c yields] kind] == "pause event"} {'>
lock_<"[$t name]">_<"[$comp name]">:=0; first_exec:= false; <'}'>to <"[[$c yields] name]">_<'}
} else {'><"\n\t">select<'
foreach y [$c yields] {'><"\n\t"><'
if {$y!=[lindex [$c yields] 0]} {'>[] <'}
if {[$y name]=="ether"} {'>endperm_<"[$t name]">_<"[$comp name]">:=true; lock_<"[$t name]">_<"[$comp name]">:= 0; to ether_<'
} else {
if {[$y kind] == "pause event"} {'>
lock_<"[$t name]">_<"[$comp name]">:=0; first_exec:= false; <'}'>to <"[$y name]">_<'
}}'><"\n\t">end<'}}}}
set last [$t name]}'><"\n">

/* services managed by <"[$t name]">*/

<'foreach s [$t services] {
set tempcodels [list]
set stopsite 0'><"\n\n">process <"[$s name]">_<"[$t name]">_<"[$comp name]"> (instance: 0..Max_<"[$comp name]">, &turn_<"[$t name]">_<"[$comp name]">: dynamicservice_<"[$comp name]">, &finished_<"[$t name]">_<"[$comp name]">: bool, &lock_<"[$t name]">_<"[$comp name]">:0..1<'
if {$codelnumber} {'>, &DMutex_<"[$comp name]">: Mutex_<"[$comp name]"><'}
if {[llength [dotgen components]]>1} {'>, &Ports: PortsArray<'}'>) is	  
states <'foreach c [$s codels] {
set ports [list]
if {[llength [dotgen components]]>1} {
	set ports [mutex-ports dotgen $c]}
foreach tr [$c triggers] {
if {!$stopsite && ([$tr name]=="stop")} {
set stopsite 1}
if {![llength [$c mutex]]  && ![llength $ports]} {
lappend tempcodels [join [list [$tr name] ""] _]
} else {lappend tempcodels [join [list [$tr name] ""] _] [join [list [$tr name] "2"] _]}}}
foreach cod $tempcodels {'><"$cod"><'
if {$cod != [lindex $tempcodels end]} {'>, <'}}'><"\n">var label: service_<"[$comp name]">:= <"[$s name]">_<"[[$s task] name]">_<"[$comp name]"><'
foreach c [$s codels] {
foreach tr [$c triggers] {'><"\n\n">from <'
set ports [list]
if {[llength [dotgen components]]>1} {
	set ports [mutex-ports dotgen $c]}
if {![llength [$c mutex]]  && ![llength $ports]} {'><"[$tr name]">_<"\n\t">on (turn_<"[$t name]">_<"[$comp name]">.name = label and turn_<"[$t name]">_<"[$comp name]">.instance=instance and lock_<"[$t name]">_<"[$comp name]">=1);/* thread-safe codel */<"\n\t"><'
if {[$tr name]!="stop"} {'>if turn_<"[$t name]">_<"[$comp name]">.current=STOP_<"[$comp name]"> then wait [0,0];<'
if {!$stopsite} {'> lock_<"[$t name]">_<"[$comp name]">:=0; finished_<"[$t name]">_<"[$comp name]">:=true; to start_ /* absence of stop codel.. directly to ether */<'
} else {'> to stop_<'}'><"\n\t">else <'}'><"\n\t">wait <'
if {![catch {$c wcet}]} {'>]0,<"[[$c wcet] value]">]; <'
} else {'>[0,0]; <'}
if {[llength [$c yields]]==1} {
if {[[$c yields] name]=="ether"} {'>finished_<"[$t name]">_<"[$comp name]">:=true; lock_<"[$t name]">_<"[$comp name]">:=0; to start_<'
} else {
if {[[$c yields] kind] == "pause event"} {'>
lock_<"[$t name]">_<"[$comp name]">:=0; <'}'>to <"[[$c yields] name]">_<'}
} else {'><"\n\t">select<'
foreach y [$c yields] {'><"\n\t"><'
if {$y!=[lindex [$c yields] 0]} {'>[] <'}
if {[$y name]=="ether"} {'>finished_<"[$t name]">_<"[$comp name]">:=true; lock_<"[$t name]">_<"[$comp name]">:=0; to start_<'
} else {
if {[$y kind] == "pause event"} {'>
lock_<"[$t name]">_<"[$comp name]">:=0; <'}'>to <"[$y name]">_<'
}}'><"\n\t">end<'}
if {[$tr name]!="stop"} {'><"\n\t">end<'}'><"\n"><'


} else {'><"[$tr name]">_<"\n\t">wait [0,0]; on (turn_<"[$t name]">_<"[$comp name]">.name = label and turn_<"[$t name]">_<"[$comp name]">.instance=instance and lock_<"[$t name]">_<"[$comp name]">=1<'
if {[$tr name]!="stop"} {'>);<"\n\t">if turn_<"[$t name]">_<"[$comp name]">.current=STOP_<"[$comp name]"> then<'
if {!$stopsite} {'> lock_<"[$t name]">_<"[$comp name]">:=0; finished_<"[$t name]">_<"[$comp name]">:=true; to start_ /* absence of stop codel.. directly to ether */<'
} else {'> to stop_<'}'><"\n\t">else on (<'
} else {'> and <'}
	
set mutex [mutex-indexes $comp $c]
foreach m $mutex {
	if {$m != [lindex $mutex end]} {'>not DMutex_<"[$comp name]">[<"[lindex $m 0]">]/*incmopatible with <"[lindex $m 1]">_<"[lindex $m 2]">*/<'
	if {$m != [lindex $mutex end-1]} {'> and <'}}}
	if {[llength [$c mutex]] && [llength $ports]} {'> and <'}
foreach p $ports {'>not Ports[<"[lindex $p 0]">] /* uses (<"[lindex $p 1]">) the port <"[lindex $p 2]"> of the component <"[lindex $p 3]"> */<'
	if {$p!=[lindex $ports end]} {'> and <'}}'>);<'
	if {[llength [$c mutex]]} {'> DMutex_<"[$comp name]">[<"$m">]:= true;<'}
	foreach p $ports {'> Ports[<"[lindex $p 0]">]:= true;<'}'><"\n\t">to <"[$tr name]">_2<'
if {[$tr name]!="stop"} {'><"\n\t">end<'}'>
<"\n">from <"[$tr name]">_2<"\n\t">wait<'
if {![catch {$c wcet}]} {'>]0,<"[[$c wcet] value]">]; <'
} else {'>[0,0]; <'}
if {[llength [$c mutex]]} {'>DMutex_<"[$comp name]">[<"$m">]:= false; <'}
	foreach p $ports {'>Ports[<"[lindex $p 0]">]:= false; <'}
if {[llength [$c yields]]==1} {
if {[[$c yields] name]=="ether"} {'>finished_<"[$t name]">_<"[$comp name]">:=true; lock_<"[$t name]">_<"[$comp name]">:=0; to start_<'
} else {
if {[[$c yields] kind] == "pause event"} {'>
lock_<"[$t name]">_<"[$comp name]">:=0; <'}'>to <"[[$c yields] name]">_<'}
} else {'><"\n\t">select<'
foreach y [$c yields] {'><"\n\t"><'
if {$y!=[lindex [$c yields] 0]} {'>[] <'}
if {[$y name]=="ether"} {'>finished_<"[$t name]">_<"[$comp name]">:=true; lock_<"[$t name]">_<"[$comp name]">:= 0; to start_<'
} else {
if {[$y kind] == "pause event"} {'>
lock_<"[$t name]">_<"[$comp name]">:=0; <'}'>to <"[$y name]">_<'
}}'><"\n\t">end<'}}}}}}'>


/* Main component */
	
component <"[$comp name]"><'
if {[llength [dotgen components]]>1} {'>(&Ports: PortsArray<'
if {$argv} {'>, &Cores: 0..CoresNumber<'}'>)<'}'> is
var <'
if {$argv && [llength [dotgen components]]<2} {'>Cores: 0..CoresNumber:= CoresNumber, <'}
if {$codelnumber} {'>DMutex_<"[$comp name]">: Mutex_<"[$comp name]">:= [<'
for {set k 1} {$k <= $codelnumber} {incr k} {'>false<'
	if {$k < $codelnumber} {'>, <'}}'>], <'}'>
execution_<"[$comp name]">:staticScheduler_<"[$comp name]"> := [<'
foreach a $activities {
set incomp 0
foreach inc $selfincompatible {
if {$a == $inc} {
set incomp 1'>{name=<"[$a name]">_<"[[$a task] name]">_<"[$comp name]">, current=VOID_<"[$comp name]">, instance=1}, {name=<"[$a name]">_<"[[$a task] name]">_<"[$comp name]">, current=VOID_<"[$comp name]">, instance=2}, <'
break}}
if {$incomp==0} {
for {set k 1} {$k <= $max} {incr k} {'>{name=<"[$a name]">_<"[[$a task] name]">_<"[$comp name]">, current=VOID_<"[$comp name]">, instance=<"$k">}, <'}}}'>{name=None_<"[$comp name]">, current=VOID_<"[$comp name]">, instance=0}],<'
foreach t [$comp tasks] {'> turn_<"[$t name]">_<"[$comp name]">: dynamicservice_<"[$comp name]">:={name=None_<"[$comp name]">, current=VOID_<"[$comp name]">, instance=0}, lock_<"[$t name]">_<"[$comp name]">: 0..1:= 0,<'
if {[llength [$t services]]} {'> finished_<"[$t name]">_<"[$comp name]">: bool:= false,<'}
if {[llength [$t codels]]} {'> endperm_<"[$t name]">_<"[$comp name]">: bool:= false,<'}
if {![catch {$t period}]} {'> tick_<"[$t name]">_<"[$comp name]">: bool:= false,<'}}'> shut_<"[$comp name]">: bool:= false<'
if {[llength $activities]} {'>, sched_<"[$comp name]">: bool:= false<'}'>

port<"\n\t"><'
if {$lastperm} {

foreach t [$comp tasks] {
	
	if {![catch {$t period}]} {'>
		shuttimer_<"[$t name]">_<"[$comp name]">: sync in [0,0],<'}
	
	if {[llength [$t codels]]} {'>
		
		end_spawn_<"[$t name]">_<"[$comp name]">: sync in <'
		if {![catch {[lindex [$t codels] 0] wcet}]} {'>]0, <"[[[lindex [$t codels] 0] wcet] value]">]<'
		} else {'>[0,0]<'}'>,
		
<'}}}
if {[llength $activities]} {'>reqimm_<"[$comp name]">: service_<"[$comp name]"> in [0,0],<"\n\t">Creqimm_<"[$comp name]">: CR_<"[$comp name]"> in [0,0],<"\n\t"><'}
if {[llength $services]} {'>req_<"[$comp name]">: service_<"[$comp name]">,<"\n\t"><'}'>Creq_<"[$comp name]">: CR_<"[$comp name]">

par * in<"\n\t"><'
if {[llength [dotgen components]] == 1} {'>client_<"[$comp name]">[<'
if {[llength $services]} {'>req_<"[$comp name]">, <'}'>Creq_<"[$comp name]"><'
if {[llength $activities]} {'>, reqimm_<"[$comp name]">, Creqimm_<"[$comp name]"><'}'>]<"\n\t">||<'}'> CT_<"[$comp name]">[<'
if {$lastperm} {'>end_spawn_<"[[lindex [$comp tasks] [expr $lastperm - 1]] name]">_<"[$comp name]">, <'}
if {[llength $services]} {'>req_<"[$comp name]">, <'}'>Creq_<"[$comp name]"><'
if {[llength $activities]} {'>, reqimm_<"[$comp name]">, Creqimm_<"[$comp name]"><'}'>] (<'
if {$argv} {'>&Cores, <'}
if {[llength $services]} {'>&execution_<"[$comp name]">, <'
if {[llength $activities]} {'>&sched_<"[$comp name]">, <'}}'>&shut_<"[$comp name]"><'
if {$codelnumber} {'>, &DMutex_<"[$comp name]"><'}
if {[llength [dotgen components]]>1} {'>, &Ports<'}'>)<'
set last "noperm"
foreach t [$comp tasks] {
if {![catch {$t period}]} {'><"\n\t">|| timer_<"[$t name]">_<"[$comp name]"> [shuttimer_<"[$t name]">_<"[$comp name]"><'
if {$last != "noperm"} {'>, end_spawn_<"$last">_<"[$comp name]"><'}'>](&tick_<"[$t name]">_<"[$comp name]">)<'}'>

<"\t">|| Taskmanager_<"[$t name]">_<"[$comp name]"> <'
if {![catch {$t period}]} {'>[shuttimer_<"[$t name]">_<"[$comp name]">]<'}'> (&turn_<"[$t name]">_<"[$comp name]"><'
if {[llength [$t services]]} {'>, &execution_<"[$comp name]">, &finished_<"[$t name]">_<"[$comp name]">, &sched_<"[$comp name]"><'}
if {$argv} {'>, &Cores<'}
if {![catch {$t period}]} {'>, &tick_<"[$t name]">_<"[$comp name]"><'}
if {[llength [$t codels]]} {'>, &endperm_<"[$t name]">_<"[$comp name]"><'}'>, &lock_<"[$t name]">_<"[$comp name]">, &shut_<"[$comp name]">)<'
if {[llength [$t codels]]} {'><"\n\t">|| Task_<"[$t name]">_<"[$comp name]"> [end_spawn_<"[$t name]">_<"[$comp name]"><'
if {$last != "noperm"} {'>, end_spawn_<"$last">_<"[$comp name]"><'}'>] (&lock_<"[$t name]">_<"[$comp name]">, &endperm_<"[$t name]">_<"[$comp name]">, &turn_<"[$t name]">_<"[$comp name]"><'
if {$codelnumber} {'>, &DMutex_<"[$comp name]"><'}
if {[llength [dotgen components]]>1} {'>, &Ports<'}'>)<'
set last [$t name]}

foreach s [$t services] {
set incomp 0
foreach inc $selfincompatible {
if {$s == $inc} {
set incomp 1'><"\n\t">|| <"[$s name]">_<"[$t name]">_<"[$comp name]"> (1, &turn_<"[$t name]">_<"[$comp name]">, &finished_<"[$t name]">_<"[$comp name]">, &lock_<"[$t name]">_<"[$comp name]"><'
if {$codelnumber} {'>, &DMutex_<"[$comp name]"><'}
if {[llength [dotgen components]]>1} {'>, &Ports<'}'>)<"\n\t">||<"[$s name]">_<"[$t name]">_<"[$comp name]"> (2, &turn_<"[$t name]">_<"[$comp name]">, &finished_<"[$t name]">_<"[$comp name]">, &lock_<"[$t name]">_<"[$comp name]"><'
if {$codelnumber} {'>, &DMutex_<"[$comp name]"><'}
if {[llength [dotgen components]]>1} {'>, &Ports<'}'>)<'
break}}
if {$incomp==0} {
for {set k 1} {$k <= $max} {incr k} {'><"\n\t">|| <"[$s name]">_<"[$t name]">_<"[$comp name]"> (<"$k">, &turn_<"[$t name]">_<"[$comp name]">, &finished_<"[$t name]">_<"[$comp name]">, &lock_<"[$t name]">_<"[$comp name]"><'
if {$codelnumber} {'>, &DMutex_<"[$comp name]"><'}
if {[llength [dotgen components]]>1} {'>, &Ports<'}'>)<'}}}}'>

end<'}
if {[llength [dotgen components]]==1} {'>

/* Entry point for verification */

<"[$comp name]"><'

} else {'><"\n\n">component all is
var Ports: PortsArray:= [<'
for {set k 1} {$k <= $portsnumber} {incr k} {'>false<'
	if {$k < $portsnumber} {'>, <'}}'>]<'
if {$argv} {'>, Cores: 0..CoresNumber:= CoresNumber<'}'>
<"\n">par<'foreach comp [dotgen components] {'><"\n\t"><'
if {$comp != [lindex [dotgen components] 0]} {'>|| <'}'><"[$comp name]">(&Ports<'
if {$argv} {'>, &Cores<'}'>)<'}'><"\n">end<"\n\n">/* Entry point for verification */<"\n\n">all
<'}'>
