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

<'
if {[llength [dotgen components]]==1} {
set comp [lindex [dotgen components] 0]
set numbservpertask [list]
set max 2
set lengthinterr [list]
set async [list]
set selfincompatible [list]
set services [list]
set numbincompertask [list]
set activities [list]
set functions [list]
set attributes [list]
foreach t [$comp tasks] {
set temp 0
set numberincomp 0
set asyncserv 0
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
if {[$s kind]=="function"} {
lappend functions $s
} else {
if {[$s kind]=="attribute"} {
lappend attributes $s}}}}
set services [concat $activities $functions $attributes]
set maxint 0
if {[llength $lengthinterr]!=0} {
set maxint [lindex $lengthinterr 0]
foreach l $lengthinterr {
if {$l > $maxint} {
set maxint $l}}}

'>

/* This module is automatically generated. Services codels' temporization is set according to their respective WCET
										     Mohammed Foughali (LAAS, May 2015) */
/* constants & types */<'
if {[llength $activities]!=0} {'>

const Max: nat is <"$max"> /* maximum of instances allowed per one self-compatible service, freely changeable */
<'set index -1
set widths [list]
set indexes [list]
foreach t [$comp tasks] {
incr index'>
const Nbactivities_<"[$t name]">: nat is <"[lindex $numbservpertask $index]">/* number of services for <"[$t name]"> task*/
const SelfIncomp_<"[$t name]">: 0..Nbactivities_<"[$t name]"> is <"[lindex $numbincompertask $index]">/* number of self-incompatible services */<'
set width [expr [lindex $numbincompertask $index]*2+$max* [expr [lindex $numbservpertask $index]-[lindex $numbincompertask $index]]]
lappend widths $width
}
set index 0
foreach w $widths {
if {$index==0} {
lappend indexes $index
} else {
lappend indexes [expr [lindex $indexes [expr $index-1]]+[lindex $widths [expr $index-1]]]}
incr index}'><"\n">const maxinterrupts: nat is <"$maxint"> /* maximum length of the interrupts queue */<'}
if {[llength $services]!=0} {'>

const Nbnonact: nat is <"[expr [llength $functions] + [llength $attributes]]"> /* number of attributes and functions */<'}
if {[llength $activities]!=0} {
foreach t [$comp tasks] {'>
<"\n">const width_<"[$t name]">: nat is SelfIncomp_<"[$t name]">*2+(Nbactivities_<"[$t name]">-SelfIncomp_<"[$t name]">)*Max<'}'><"\n">const width: nat is <'set index 1
foreach t [$comp tasks] {'>width_<"[$t name]"><'
if {$t!=[lindex [$comp tasks] end]} {'>+<'}}'>/* size of the activities part*/<'}
if {[llength $services]!=0} {
if {[llength $activities]!=0} {'><"\n">const size: nat is width+Nbnonact+1<'
} else {'><"\n">const size: nat is Nbnonact+1<'}}'><"\n">type service is union <'foreach s $services {'>
<"[$s name]"><'
if {[$s kind]=="activity"} {'>_<"[[$s task] name]"><'}'> | <'}'>
None end /* possible names of services */<'
if {[llength $activities]!=0} {'>

type servicetype is union Attribute | Function | Activity end<'}'><"\n">type status is union  VOID /* inactive */ |START /*waiting*/ | RUNNING /*active*/ | STOP /*to pass to the stop state*/ | STOPPING /*stopping*/ | STOPPED /*ended after being interrupted */ | ETHER end<"\n">type dynamicservice is record name: service, current: status, instance: 0..<'
if {[llength $activities]!=0} {'>Max<'
} else {'>0<'}'> end /* type of the "turn" shared variables */<'
if {[llength $services]!=0} {'>

type staticScheduler is array size of dynamicservice /* type of the execution array */<'}
if {[llength $activities]!=0} {'>

type Interrupts is queue maxinterrupts of service /* field "interrupts" of a GenoM service */
type IndexA is 0..width-1<'}
if {[llength $services]!=0} {'>

type Index is 0..size-1<'}'><"\n">type CRName is union Kill<'
if {[llength $activities]!=0} {'> | Abort<'}'> end
type CR is record name: CRName, who: <'
if {[llength $activities]!=0} {'>IndexA<'
} else {'>0..0<'}'> end
type CRreply is union DONE | NOSUCHACT end
<'if {[llength $activities]!=0} {
set index 0
foreach t [$comp tasks] {
if {[llength [$t services]]!=0} {'>
const index_<"[$t name]">: IndexA is <"[lindex $indexes $index]"><'
incr index}}}'>


/* functions */<'
if {[llength $activities]!=0} {'>


/* is an execution task runnable? */
function running (execution: staticScheduler, index: Index, widthtask: nat): bool is
var limit: nat
begin
	limit:= index+widthtask;
	while index<limit do
		if not (execution[index].current=STOPPED or execution[index].current=ETHER or execution[index].current=VOID) then return true end;
		index:= index+1
	end; return false
end

/* service type */
function servtype (serv: service): servicetype is
begin<'
if {[llength $functions]!=0} {'><"\n\t">if (<'
foreach f $functions {'>serv=<"[$f name]"><'
if {$f == [lindex $functions end]} {'>)<'
} else {'> or <'}}'> then return Function end;<'}
if {[llength $attributes]!=0} {'><"\n\t">if (<'
foreach a $attributes {'>serv=<"[$a name]"><'
if {$a == [lindex $attributes end]} {'>)<'
} else {'> or <'}}'> then return Attribute end;<'}'><"\n\t">return Activity 
end

/* services that I interrupt */
function Iinterrupt (serv: service): Interrupts is
begin
	case serv of<'set first 0
if {[llength $services]!=0} {
foreach s $services {
if {[llength [$s interrupts]]!=0} {'><"\n\t"><'
if {$first!=0} {'>| <'}'>
<"[$s name]"><'if {[$s kind]=="activity"} {'>_<"[[$s task] name]"><'}'> -> return {|<'
set first 1
if {[llength [$s interrupts]]==[llength [$comp services]]} {
foreach a $activities {'>
<"[$a name]">_<"[[$a task] name]"><'if {$a != [lindex $activities end]} {'>,<'}}
} else {
foreach i [$s interrupts] {'>
<"[$i name]">_<"[[$i task] name]"><'if {$i != [lindex [$s interrupts] end]} {'>,<'}}}'>|} <'}}'><"\n\t"><'
if {$first!=0} {'>| <'}}'> any -> return {||}
	end
end

/* check for incompatibility */
function incomp (interrupts: Interrupts, serv: service) : bool is
begin
    if empty (interrupts) then return false end;
    if first (interrupts) = serv then return true end;
    return incomp (dequeue (interrupts), serv)
end

/* returns indexes of incompatible services */
function sitesincomp (serv: service, execution: staticScheduler, indexx: Index): queue width of IndexA is
var index: IndexA:=0, sites: queue width of IndexA:= {||}
begin
	foreach index do
		if not (index=indexx) then
			if incomp(Iinterrupt(serv), execution[index].name) then
				sites:= enqueue(sites, index)
			end
		end
	end; return sites
end

/* manage interrupts */
function manageinterrupt (execution: staticScheduler, index: Index): status is
begin
	if execution[index].current= START then return STOPPED end;
	if execution[index].current= RUNNING then return STOP end;
	return execution[index].current
end

/* clear */
function clear (execution: staticScheduler, index: Index): status is
begin
	if (execution[index].current=STOPPED or execution[index].current=ETHER) then return VOID end;
	return execution[index].current
end

/* unblock the services line */
function next (execution: staticScheduler, index: Index, indextask: IndexA, widthtask: nat): Index is
var limit: nat
begin
	limit:= indextask+widthtask;
	while index<limit do 
		if (execution[index].current=RUNNING or execution[index].current=STOPPING or execution[index].current=STOP) then 
		   return index
		end;
		index:= index+1 
	end; return $(size-1)
end<'}
if {[llength $services]!=0} {'>


/* validate or stateless */
function needsvalidate (serv: service): bool is
begin<'
set count 0 
set noval [list] 
foreach s $services {
if {[llength [$s validate]] == 0} {
incr count
lappend noval $s}}
if {$count==0} {'><"\n\t">return true <'
} elseif {$count==[llength $services]} {'><"\n\t">return false <'
} else {'><"\n\t">if (<'
foreach serv $noval {'>serv=<"[$serv name]"><'
if {[$serv kind]=="activity"} {'>_<"[[$serv task] name]"><'}
if {$count>1} {'> or <'incr count -1} else {'>)<'} }'> then return false end;<"\n\t">return true <'
}'><"\n">end 

/* look for a free slot. Optimized: we no longer check all the array elements */
function multipleinstances (execution: staticScheduler, temp: service) : Index is
var index: Index := 0
begin
	foreach index do 
		if execution[index].name=temp then
			if not (servtype(temp)=Activity) then return index end; 
			while (index<width and execution[index].name=temp) do 
				if execution[index].current=VOID then return index end;
				index:=index+1
			end;
			return $(size-1)
		end
	end;
	return $(size-1) /* never reached, but the compiler complains if omitted */
end<'}'>


/* Processes */

/* client */

process client [<'
if {[llength $services]!=0} {'>req: out service, <'}'>Creq: out CR,<'
if {[llength $activities]!=0} {'> reqimm: out service, Creqimm: out CR, intrep: in Index,<'}
if {[llength $services]!=0} {'> finalrep: in sync,<'}'> Creply: in CRreply] is
states start, getrep
var <'if {[llength $activities]!=0} {'>temp: Index, c: IndexA, <'}'>tempCrep: CRreply

from start <'if {[llength $services]!=0} {'><"\n\t">select <'
foreach s $services {'><"\n\t"><'
if {$s != [lindex $services 0]} {'>[] <'}'>req!<"[$s name]"><'
if {[$s kind]=="activity"} {'>_<"[[$s task] name]"><'}}
if {[llength $activities]!=0} {
foreach s $services {'><"\n\t">[] reqimm!<"[$s name]"><'
if {[$s kind]=="activity"} {'>_<"[[$s task] name]"><'}}'><"\n\t">[] reqimm!None<'}'><"\n\t">[] Creq!{name=Kill, who=0}<'
if {[llength $activities]!=0} {'><"\n\t">[] c:= any; Creq!{name=Abort, who=c}
	[] Creqimm!{name=Kill, who=0}
	[] c:= any; Creqimm!{name=Abort, who=c}<'}'><"\n\t">end; to getrep<'
} else {'>Creq!{name=Kill, who=0}; to getrep<'}'>


from getrep<'if {[llength $services]!=0} {'> select <'
if {[llength $activities]!=0} {'><"\n\t\t">intrep?temp; to getrep
		[]<'}'> Creply?tempCrep; to getrep
		[] finalrep; to start
	    end<'
} else {'> Creply?tempCrep; to getrep<'}'>

/* control task */

process CT[<'
if {[llength $services]!=0} {'>req: in service, <'}'>Creq: in CR,<'
if {[llength $activities]!=0} {'> reqimm: in service, Creqimm: in CR, intrep: out Index,<'}
if {[llength $services]!=0} {'> finalrep: out sync,<'}'> Creply: out CRreply] (<'
if {[llength $services]!=0} {'>&execution: staticScheduler, <'}'>&lock:0..<'
if {[llength $activities]!=0} {'>2, <'
} else {'>1, <'}'>&shut: bool<'
if {[llength $activities]!=0} {'>, &sched: bool<'}'>) is
states start,<'
if {[llength $services]!=0} {'> manage, finish,<'}'> manageCR, shutdown
var <'
if {[llength $services]!=0} {'>index: Index, temp: service, <'}
if {[llength $activities]!=0} {'>index2: IndexA, line: queue width of 0..size-1, launcher: bool:= true, <'}'>tempCR: CR

from start on lock=0; lock:=1;<'
if {[llength $services]!=0} {'> index:=0;<'}'><"\n"><'
if {[llength $activities]!=0} {'>
	if sched then sched:= false; 
	select	reqimm?temp; if temp=None then to finish else to manage end
	     [] Creqimm?tempCR; to manageCR
	end
 	else<'}'><"\n"><'
if {[llength $services]!=0} {'>
	select	req?temp; if temp=None then to finish else to manage end
	     [] Creq?tempCR; to manageCR
	end<'
} else {'>
	Creq?tempCR; to manageCR<'}
if {[llength $activities]!=0} {'><"\n\t">end<'}
if {[llength $services]!=0} {'>  

from manage 
	index:= multipleinstances(execution,temp);
	if index=size-1 then wait [0,0]; to finish
	else
		if needsvalidate(temp) then
			select execution[index].current:= VOID
	    	       [] execution[index].current:= START 
			end
		else
			execution[index].current:= START
		end;
		if execution[index].current= VOID then wait[0,0] 
		else<'
if {[llength $activities]!=0} {'><"\n">
    		line:= sitesincomp(temp, execution, index);	
			while not (empty line) do
				execution[first line].current:= manageinterrupt(execution, first line);
				if execution[first line].current=STOP or execution[first line].current=STOPPING then launcher:=false end;
				line:= dequeue line
			end;
			if index<width then
				if launcher then execution[index].current:= RUNNING end;
			/* send intermediate reply */
				intrep!index
			else<'}'>
			/* execute the non-activity here */
			wait [0,0];
			execution[index].current:= VOID<'
if {[llength $activities]!=0} {'><"\n\t\t\t">end<'}'><"\n\t\t">end
	end; to finish<'}'>


from manageCR <'
if {[llength $activities]!=0} {'><"\n\t">if tempCR.name=Kill then
		foreach index2 do
			execution[index2].current:= manageinterrupt (execution, index2)
		end;
		Creply!DONE; shut:= true; lock:= 0; to shutdown
	else 	if execution[tempCR.who].current=VOID then Creply!NOSUCHACT
		else execution[tempCR.who].current:= manageinterrupt (execution, tempCR.who);
		     Creply!DONE
		end; to finish
	end<'
} else {'>Creply!DONE; shut:= true; lock:= 0; to shutdown<'}
if {[llength $services]!=0} {'>


from finish /* final replies */<'
if {[llength $activities]!=0} {'>
	/* process pending activities */
	launcher:= true;
	foreach index2 do
		if execution[index2].current=START then
			line:= sitesincomp(execution[index2].name, execution, index2);
			while not (empty line) do
				execution[first line].current:= manageinterrupt(execution, first line);
				if (execution[first line].current=STOP or execution[first line].current=STOPPING) then launcher:= false end; 
			line:= dequeue line
			end;
			if launcher then execution[index2].current:= RUNNING end;
			launcher:= true
		end
	end;	 
	/* update final replies */
	foreach index2 do
		execution[index2].current:= clear (execution, index2)
	end;	
	<'}'><"\n\t">finalrep; lock:= 0; to start<'}

set counterG 0
foreach t [$comp tasks] {
set oneatleastG($counterG) 0
set TS [list]
foreach c [$t codels] {
lappend TS 0}
set counter -1
set oneatleast($counterG) 0
foreach c [$t codels] {
incr counter
set test [thread-safe $comp $c]
if {!$test} {continue}
lset TS $counter 1
if {!$oneatleastG($counterG)} {incr oneatleastG($counterG)}
if {!$oneatleast($counterG)} {incr oneatleast($counterG)}}

set counter -1
set oneatleastS($counterG) [list]
foreach s [$t services] {
lappend oneatleastS($counterG) 0}
foreach s [$t services] {
incr counter
set TSS($counter) [list]
foreach c [$s codels] {
lappend TSS($counter) 0}
set counter2 -1
foreach c [$s codels] {
incr counter2
set test [thread-safe $comp $c]
if {!$test} {continue}
lset TSS($counter) $counter2 1
if {!$oneatleastG($counterG)} {incr oneatleastG($counterG)}
if {![lindex $oneatleastS($counterG) $counter]} {lset oneatleastS($counterG) $counter 1}}}


if {![catch {$t period}]} {'><"\n">
/* <"[$t name]"> timer */

process timer_<"[$t name]"> (&tick_<"[$t name]">: bool, &shuttimer_<"[$t name]">: bool) is
states start, shutdown

from start
	wait[<"[[$t period] value]">, <"[[$t period] value]">];
	if shuttimer_<"[$t name]"> then to shutdown end;
	tick_<"[$t name]">:= true; to start<'}'>


/* <"[$t name]"> manager */

process Taskmanager_<"[$t name]"> (&turn_<"[$t name]"> :dynamicservice<'
if {[llength [$t services]]!=0} {'>, &execution : staticScheduler<'}
if {![catch {$t period}]} {'>, &tick_<"[$t name]">: bool, &shuttimer_<"[$t name]">: bool<'}'>, &lock:0..<'
if {[llength $activities]!=0} {'>2<'
} else {'>1<'}'>, &shut: bool<'
if {[llength [$t services]]!=0} {'>, &finished_<"[$t name]">: bool, &sched: bool<'}
if {[llength [$t codels]]!=0} {'>, &endperm_<"[$t name]">: bool<'}
if {$oneatleastG($counterG)} {'>, &async_<"[$t name]">:bool<'}'>) is
states start, shutdown<'
if {[llength [$t services]]!=0} {'>, orchestrate<"\n">
var index: Index:= 0, firstfiring: bool<'}'>
  
from start<"\n\t"><' 
if {![catch {$t period}]} {'>wait[0,0];<"\n\t"><'}'>on (<'
if {![catch {$t period}]} {'>tick_<"[$t name]"> and <'}
if {$oneatleastG($counterG)} {'>async_<"[$t name]"> and <'}'>lock=0); <'
if {![catch {$t period}]} {'>tick_<"[$t name]">:= false;<'}'><"\n\t"><'
if {[llength [$t services]]!=0} {'>if not ((running(execution, index_<"[$t name]">, width_<"[$t name]">))<'
if {[llength [$t codels]]!=0} {'> or not endperm_<"[$t name]"><'}'>)<'
} else {'>if endperm_<"[$t name]"><'}'> then 
			if shut then<'
if {![catch {$t period}]} {'> shuttimer_<"[$t name]">:= true;<'}'> to shutdown else to start end // nothing to do. lock implicitely released
	else <'
if {[llength [$t services]]==0} {'>turn_<"[$t name]">.current:= START; lock:= 1; to start<"\n\t">end<'
} else {
if {[llength [$t codels]]!=0} {'><"\n\t\t">if not (running(execution, index_<"[$t name]">, width_<"[$t name]">)) then<"\n\t\t">turn_<"[$t name]">:={name=None, current= START, instance=0};<"\n\t\t">lock:= 2; to start<"\n\t\t">else<'}'><"\n\t\t">firstfiring:= true; index:= index_<"[$t name]">; lock:=1;<"\n\t\t"><'
if {[llength [$t codels]]!=0} {'>if not endperm_<"[$t name]"> then<"\n\t\t">turn_<"[$t name]">:= {name=None, current= RUNNING, instance=0};<"\n\t\t">lock:=2 end;<"\n\t\t">to orchestrate<"\n\t\t">end<'
} else {'><"\n\n\t">to orchestrate<'}'><"\n\t">end

from orchestrate
	wait [0,0]; on <'if {$oneatleastG($counterG)} {'>(async_<"[$t name]"> and lock=1);<'
} else {'>lock=1;<'}'> 
	if not firstfiring then
		if finished_<"[$t name]"> then
			finished_<"[$t name]">:= false;
			if sched=false then sched:= true end;
			if execution[index].current=RUNNING then execution[index].current:=ETHER
			else execution[index].current:=STOPPED end
		end;
		index:= next(execution, index+1, index_<"[$t name]">, width_<"[$t name]">);
		turn_<"[$t name]">:= execution[index];
		if index>width-1 then lock:= 0;
			if shut then<'
if {![catch {$t period}]} {'> shuttimer_<"[$t name]">:= true;<'}'> to shutdown else to start end
		else
 		if execution[index].current=STOP then execution[index].current:=STOPPING end;
		lock:= 2; to orchestrate
		end
	end;
	firstfiring:= false;
	index:= next(execution, index, index_<"[$t name]">, width_<"[$t name]">);
	turn_<"[$t name]">:= execution[index];
	if execution[index].current=STOP then execution[index].current:=STOPPING end;
	lock:= 2; to orchestrate<'}
if {[llength [$t codels]]!=0} {'><"\n\n">/* <"[$t name]"> permanent activity automaton */

process Task_<"[$t name]"> (&lock: 0..<'
if {[llength $activities]!=0} {'>2, <'
} else {'>1, <'}'>&endperm_<"[$t name]">: bool, &turn_<"[$t name]">: dynamicservice<'
if {$oneatleast($counterG)} {'>, &async_<"[$t name]">:bool<'}'>) is
states <'
set counter 0
foreach c [$t codels] {
foreach tr [$c triggers] {
if {[lindex $TS $counter]} {'><"[$tr name]">_, <"[$tr name]">_2, <"[$tr name]">_3<'
} else {'><"[$tr name]">_<'}
if {$c != [lindex [$t codels] end] || $tr != [lindex [$c triggers] end]} {'>, <'}}
incr counter}
set counter 0
foreach c [$t codels] {
foreach tr [$c triggers] {
if {[lindex $TS $counter]} {'><"\n\n">/* beginning of the asynchronous codel corresponding states */<"\n">from <"[$tr name]">_<"\n\t"><'
if {[llength [$t services]]==0} {'>on (not (turn_<"[$t name]">.current=VOID) and lock=1);<'
} else {'>on (turn_<"[$t name]">.name=None and not (turn_<"[$t name]">.current=VOID) and lock=2);<'}'><"\n\t">async_<"[$t name]">:=false; lock:=0;<"\n\t">to <"[$tr name]">_2<"\n">from <"[$tr name]">_2<"\n\t">wait <'
if {![catch {$c wcet}]} {'>[<"[[$c wcet] value]">,<"[[$c wcet] value]">];<'
} else {'>[0,0];<'}'>
/* core of the async codel */ to <"\n\t"> <"[$tr name]">_3<"\n">from <"[$tr name]">_3<"\n\t">on lock=0; async_<"[$t name]">:=true;<'
if {[llength [$t services]]==0} {'>lock:=1;<"\n\t"><'
} else {'>lock:=2;<'}'><"\n\t"><'
} else {'><"\n\n">from <"[$tr name]">_<"\n\t">wait <'
if {![catch {$c wcet}]} {'>[<"[[$c wcet] value]">,<"[[$c wcet] value]">];<'
} else {'>[0,0];<'}
if {[llength [$t services]]==0} {'>on (not (turn_<"[$t name]">.current=VOID) and lock=1);<"\n\t"><'
} else {'>on (turn_<"[$t name]">.name=None and not (turn_<"[$t name]">.current=VOID) and lock=2);<"\n\t"><'}}
if {[llength [$c yields]]==1} {
if {[[$c yields] name]=="ether"} {'>endperm_<"[$t name]">:=true;<'
if {[llength [$t services]]==0} {'>turn_<"[$t name]">.current:=VOID; lock=0;<"\n\t"><'
} else {'>if turn_<"[$t name]">.current=START then turn_<"[$t name]">.current:=VOID; lock:=0 else lock:=1 end;<"\n\t"><'}'>
 to start_<'
} else {
if {[[$c yields] kind] == "pause event"} {
if {[llength [$t services]]==0} {'>turn_<"[$t name]">.current:=VOID; lock=0;<"\n\t"><'
} else {'>if turn_<"[$t name]">.current=START then turn_<"[$t name]">.current:=VOID; lock:=0 else lock:=1 end;<"\n\t"><'}}'>to <"[[$c yields] name]">_<'}
} else {'>select<'
foreach y [$c yields] {'><"\n\t"><'
if {$y!=[lindex [$c yields] 0]} {'>[] <'}
if {[$y name]=="ether"} {'>endperm_<"[$t name]">:=true;<'
if {[llength [$t services]]==0} {'>turn_<"[$t name]">.current:=VOID; lock=0;<"\n\t"><'
} else {'>if turn_<"[$t name]">.current=START then turn_<"[$t name]">.current:=VOID; lock:=0 else lock:=1 end;<"\n\t"><'}'> to start_<'
} else {
if {[$y kind] == "pause event"} {
if {[llength [$t services]]==0} {'>turn_<"[$t name]">.current:=VOID; lock=0;<"\n\t"><'
} else {'>if turn_<"[$t name]">.current=START then turn_<"[$t name]">.current:=VOID; lock:=0 else lock:=1 end;<"\n\t"><'}}'>to <"[$y name]">_<'}}'><"\n\t">end<'}}
incr counter}}'><"\n">

/* services managed by <"[$t name]">*/
/* if the codel stop is never reached explicitely (in the yields field), then no need to create a state correspoding to it*/

<'set counter 0
foreach s [$t services] {
set tempcodels [list]
set stopsite -1
set codsite 0'><"\n\n">process <"[$s name]">_<"[[$s task] name]"> (instance: 0..Max, &turn_<"[[$s task] name]">: dynamicservice, &finished_<"[[$s task] name]">: bool, &lock:0..2<'
if {[lindex $oneatleastS($counterG) $counter]} {'>, &async_<"[$t name]">:bool<'}'>) is	  
states <'set stopnec 0
set stopasync 0
foreach c [$s codels] {
foreach tr [$c triggers] {
if {[$tr name]=="stop" && [thread-safe $comp $c]} {
set stopnec 1
set stopasync 1
break}}
if {$stopnec} {break}
foreach y [$c yields] {
if {[$y name]=="stop"} {
set stopnec 1
break}}
if {$stopnec} {break}
}
set counter2 0
foreach c [$s codels] {
foreach tr [$c triggers] {
if {[$tr name]=="stop"} {
set stopsite $codsite}
if {[$tr name]!="stop" || $stopnec==1} {
if {[lindex $TSS($counter) $counter2]} {
lappend tempcodels [join [list [$tr name] ""] _] [join [list [$tr name] "2"] _] [join [list [$tr name] "3"] _]
} else {lappend tempcodels [join [list [$tr name] ""] _]}}}
incr codsite
incr counter2}
foreach cod $tempcodels {'><"$cod"><'
if {$cod != [lindex $tempcodels end]} {'>, <'}}'><"\n">var label: service:= <"[$s name]">_<"[[$s task] name]"><'
set counter2 0
foreach c [$s codels] {
foreach tr [$c triggers] {
if {[$tr name]!="stop" || $stopnec==1} {'><"\n\n">from <'
if {[lindex $TSS($counter) $counter2]} {'><"[$tr name]">_<"\n\t">on (turn_<"[[$s task] name]">.name = label and turn_<"[[$s task] name]">.instance=instance and lock=2);/* beginning of the async codel corresponding states */<"\n\t"><'
if {[$tr name]!="stop"} {'>if turn_<"[[$s task] name]">.current=STOP then <'}
if {$stopsite==-1} {'>wait [0,0]; lock:=1; finished_<"[[$s task] name]">:=true; to start_ /* absence of stop codel.. directly to ether */<'
} else {
if {[lindex $TSS($counter) $stopsite]} {'>wait [0,0]; lock:=0; async_<"[$t name]">:= false; to stop_2<'
} else {
if {![catch {[lindex [$s codels] $stopsite] wcet}]} {'>wait [<"[[[lindex [$s codels] $stopsite] wcet] value]">,<"[[[lindex [$s codels] $stopsite] wcet] value]">];<'
} else {'>wait [0,0];<'}'><"\n\t"><'
if {[llength [[lindex [$s codels] $stopsite]  yields]]==1} {
if {[[[lindex [$s codels] $stopsite] yields] name]=="ether"} {'>finished_<"[[$s task] name]">:=true; lock:=1; to start_ /* execution of the stop codel here */<'
} else {
if {[[[lindex [$s codels] $stopsite]  yields] kind] == "pause event"} {'>
lock:=1; <'}'>to <"[[[lindex [$s codels] $stopsite]  yields] name]">_<'}
} else {'>select<'
foreach y [[lindex [$s codels] $stopsite] yields] {'><"\n\t"><'
if {$y!=[[lindex [lindex [$s codels] $stopsite] yields] 0]} {'>[] <'}
if {[$y name]=="ether"} {'>finished_<"[[$s task] name]">:=true; lock:=1; to start_<'
} else {
if {[$y kind] == "pause event"} {'>
lock:=1; <'}'>to <"[$y name]">_<'}}'><"\n\t">end<'}}}
if {[$tr name]!="stop"} {'><"\n\t">else wait [0,0]; lock:=0; async_<"[$t name]">:= false end; to <"[$tr name]">_2<'}'><"\n\n">from <"[$tr name]">_2<"\n\t">wait <'
if {![catch {$c wcet}]} {'>[<"[[$c wcet] value]">,<"[[$c wcet] value]">];<'
} else {'>[0,0];<'}'> /* core of the codel */ to <"[$tr name]">_3<"\n\n">from <"[$tr name]">_3<"\n\t">wait [0,0]; on lock=0; lock:=2; async_<"[$t name]">:= true;<"\n\t"><'
if {[llength [$c yields]]==1} {
if {[[$c yields] name]=="ether"} {'>finished_<"[[$s task] name]">:=true; lock:=1; to start_<'
} else {
if {[[$c yields] kind] == "pause event"} {'>
lock:=1; <'}'>to <"[[$c yields] name]">_<'}
} else {'>select<'
foreach y [$c yields] {'><"\n\t"><'
if {$y!=[lindex [$c yields] 0]} {'>[] <'}
if {[$y name]=="ether"} {'>finished_<"[[$s task] name]">:=true; lock:=1; to start_<'
} else {
if {[$y kind] == "pause event"} {'>
lock:=1; <'}'>to <"[$y name]">_<'
}}'><"\n\t">end<'}'>/* end of the async codel corresponding states */<"\n"><'

} else {'><"[$tr name]">_<"\n\t">on (turn_<"[[$s task] name]">.name = label and turn_<"[[$s task] name]">.instance=instance and lock=2);<"\n\t"><'
if {[$tr name]!="stop"} {'>if turn_<"[[$s task] name]">.current=STOP then <'}
if {$stopsite==-1} {'>wait [0,0]; finished_<"[[$s task] name]">:=true; lock:= 1; to start_ /* absence of stop codel.. directly to ether */<'
} else {
if {$stopasync} {'>wait [0,0]; lock:=0; async_<"[$t name]">:= false; to stop_2<'
} else { 
if {![catch {[lindex [$s codels] $stopsite] wcet}]} {'>wait [<"[[[lindex [$s codels] $stopsite] wcet] value]">,<"[[[lindex [$s codels] $stopsite] wcet] value]">];<"\n\t"><'
} else {'>wait [0,0]<"\n\t">;<'}
if {[llength [[lindex [$s codels] $stopsite]  yields]]==1} {
if {[[[lindex [$s codels] $stopsite] yields] name]=="ether"} {'>finished_<"[[$s task] name]">:=true; lock:= 1; to start_ /* execution of the stop codel here */<'
} else {
if {[[[lindex [$s codels] $stopsite]  yields] kind] == "pause event"} {'>
lock:=1; <'}'>to <"[[[lindex [$s codels] $stopsite]  yields] name]">_<'}
} else {'>select<'
foreach y [[lindex [$s codels] $stopsite] yields] {'><"\n\t"><'
if {$y!=[[lindex [lindex [$s codels] $stopsite] yields] 0]} {'>[] <'}
if {[$y name]=="ether"} {'>finished_<"[[$s task] name]">:=true; lock:= 1; to start_<'
} else {
if {[$y kind] == "pause event"} {'>
lock:=1; <'}'>to <"[$y name]">_<'
}}'><"\n\t">end<'}}}
if {[$tr name]!="stop"} {'><"\n\t">else wait <'
if {![catch {$c wcet}]} {'>[<"[[$c wcet] value]">,<"[[$c wcet] value]">];<'
} else {'>[0,0];<'}'><"\n\t"><'
if {[llength [$c yields]]==1} {
if {[[$c yields] name]=="ether"} {'>finished_<"[[$s task] name]">:=true; lock:=1; to start_<'
} else {
if {[[$c yields] kind] == "pause event"} {'>
lock:=1; <'}'>to <"[[$c yields] name]">_<'}
} else {'>select<'
foreach y [$c yields] {'><"\n\t"><'
if {$y!=[lindex [$c yields] 0]} {'>[] <'}
if {[$y name]=="ether"} {'>finished_<"[[$s task] name]">:=true; lock:= 1; to start_<'
} else {
if {[$y kind] == "pause event"} {'>
lock:=1; <'}'>to <"[$y name]">_<'
}}'><"\n\t">end<'}'><"\n\t">end<'}}}}
incr counter2}
incr counter}
incr counterG}'>


/* Main component */
	
component <"[$comp name]"> is
var <'
if {[llength $services]!=0} {'>execution:staticScheduler := [<'
foreach a $activities {
set incomp 0
foreach inc $selfincompatible {
if {$a == $inc} {
set incomp 1'>{name=<"[$a name]">_<"[[$a task] name]">, current=VOID, instance=1}, {name=<"[$a name]">_<"[[$a task] name]">, current=VOID, instance=2}, <'
break}}
if {$incomp==0} {
for {set k 1} {$k <= $max} {incr k} {'>{name=<"[$a name]">_<"[[$a task] name]">, current=VOID, instance=<"$k">}, <'}}}
foreach at $attributes {'>
{name=<"[$at name]">, current=VOID, instance=0}, <'}
foreach f $functions {'>
{name=<"[$f name]">, current=VOID, instance=0}, <'}'>{name=None, current=VOID, instance=0}],<'}
set counterG 0
foreach t [$comp tasks] {'> turn_<"[$t name]">: dynamicservice:={name=None, current=VOID, instance=0},<'
if {[llength [$t services]]!=0} {'> finished_<"[$t name]">: bool:= false,<'}
if {[llength [$t codels]]!=0} {'> endperm_<"[$t name]">: bool:= false,<'}
if {![catch {$t period}]} {'> tick_<"[$t name]">: bool:= false, shuttimer_<"[$t name]">: bool:= false,<'}
if {$oneatleastG($counterG)} {'> async_<"[$t name]">: bool:= true,<'}
incr counterG}'> shut: bool:= false, lock: 0..<'
if {[llength $activities]!=0} {'>2<'
} else {'>1<'}'>:= 0<'
if {[llength $activities]!=0} {'>, sched: bool:= false<'}'>

port<"\n\t"><'
if {[llength $activities]!=0} {'>reqimm: service in [0,0],<"\n\t">Creqimm: CR in [0,0],<"\n\t">intrep: Index in [0,0],<"\n\t"><'}
if {[llength $services]!=0} {'>req: service,<"\n\t">finalrep: sync in [0,0],<"\n\t"><'}'>Creq: CR,<"\n\t">Creply: CRreply in [0,0]

par<"\n\t"><'
if {[llength $services]!=0} {'>req, <'}'>Creq, <'
if {[llength $activities]!=0} {'>reqimm, Creqimm, intrep, <'}
if {[llength $services]!=0} {'>finalrep, <'}'>Creply -> client[<'
if {[llength $services]!=0} {'>req, <'}'>Creq, <'
if {[llength $activities]!=0} {'>reqimm, Creqimm, intrep, <'}
if {[llength $services]!=0} {'>finalrep, <'}'>Creply]<"\n\t">|| <'
if {[llength $services]!=0} {'>req, <'}'>Creq, <'
if {[llength $activities]!=0} {'>reqimm, Creqimm, intrep, <'}
if {[llength $services]!=0} {'>finalrep, <'}'>Creply -> CT[<'
if {[llength $services]!=0} {'>req, <'}'>Creq, <'
if {[llength $activities]!=0} {'>reqimm, Creqimm, intrep, <'}
if {[llength $services]!=0} {'>finalrep, <'}'>Creply] (<'
if {[llength $services]!=0} {'>&execution, <'}'>&lock, &shut<'
if {[llength $activities]!=0} {'>, &sched<'}'>)<'
set counterG 0
foreach t [$comp tasks] {
if {![catch {$t period}]} {'><"\n\t">|| timer_<"[$t name]"> (&tick_<"[$t name]">, &shuttimer_<"[$t name]">)<'}'><"\n\t">|| Taskmanager_<"[$t name]"> (&turn_<"[$t name]"><'
if {[llength [$t services]]!=0} {'>, &execution<'}
if {![catch {$t period}]} {'>, &tick_<"[$t name]">, &shuttimer_<"[$t name]"><'}'>, &lock, &shut<'
if {[llength [$t services]]!=0} {'>, &finished_<"[$t name]">, &sched<'}
if {[llength [$t codels]]!=0} {'>, &endperm_<"[$t name]"><'}
if {$oneatleastG($counterG)} {'>, &async_<"[$t name]"><'}'>)<'if {[llength [$t codels]]!=0} {'><"\n\t">|| Task_<"[$t name]">(&lock, &endperm_<"[$t name]">, &turn_<"[$t name]"><'
if {$oneatleast($counterG)} {'>, &async_<"[$t name]"><'}'>)<'}
set counter 0
foreach s [$t services] {
set async 0
if {[lindex $oneatleastS($counterG) $counter]} {
set async 1}
set incomp 0
foreach inc $selfincompatible {
if {$s == $inc} {
set incomp 1'><"\n\t">|| <"[$s name]">_<"[[$s task] name]"> (1, &turn_<"[[$s task] name]">, &finished_<"[[$s task] name]">, &lock<'
if {$async==1} {'>, &async_<"[$t name]"><'}'>)<"\n\t">|| <"[$s name]">_<"[[$s task] name]"> (2, &turn_<"[[$s task] name]">, &finished_<"[[$s task] name]">, &lock<'
if {$async==1} {'>, &async_<"[$t name]"><'}'>)<'
break}}
if {$incomp==0} {
for {set k 1} {$k <= $max} {incr k} {'><"\n\t">|| <"[$s name]">_<"[[$s task] name]"> (<"$k">, &turn_<"[[$s task] name]">, &finished_<"[[$s task] name]">, &lock<'if {$async==1} {'>, &async_<"[$t name]"><'}'>)<'}}
incr counter}
incr counterG}'>

end

/* Entry point for verification */

<"[$comp name]"><'

} else {'> 

/* This multi-module specification is automatically generated. Services codels' temporization is set to their respective WCET.
connected ports are represented by a hybrid array (of booleans & integers). Codels writing/reading a port are commented conveniently to help formulate the properties.
																						Mohammed Foughali (LAAS, May 2015) */
										     
<'foreach comp [dotgen components] {
set inportscounter 0
set inports [list]
set externalports [list]
set counters [list]
foreach p [$comp ports] {
if {[$p dir]!="out" && [$p name]!="genom_state"} {
incr inportscounter
lappend inports $p}}
if {$inportscounter > 0} {
foreach inp $inports {
set localcount 0
foreach othercomp [dotgen components] {
if {$othercomp != $comp} {
foreach p [$othercomp ports] {
if {[$p dir] != "in" && [$p name] == [$inp name]} {
lappend externalports $p
incr localcount}}}}
lappend counters $localcount}
}
set numbservpertask [list]
set max 2
set lengthinterr [list]
set selfincompatible [list]
set services [list]
set numbincompertask [list]
set activities [list]
set functions [list]
set attributes [list]
foreach t [$comp tasks] {
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
if {[$s kind]=="function"} {
lappend functions $s
} else {
if {[$s kind]=="attribute"} {
lappend attributes $s}}}}
set services [concat $activities $functions $attributes]
set maxint 0
if {[llength $lengthinterr]!=0} {
set maxint [lindex $lengthinterr 0]
foreach l $lengthinterr {
if {$l > $maxint} {
set maxint $l}}}

'> 

/* constants & types */<'
if {[llength $activities]!=0} {'>

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
if {[llength $services]!=0} {'>

const Nbnonact_<"[$comp name]">: nat is <"[expr [llength $functions] + [llength $attributes]]"> /* number of attributes and functions */<'}
if {[llength $activities]!=0} {
foreach t [$comp tasks] {'><"\n">const width_<"[$t name]">_<"[$comp name]">: nat is SelfIncomp_<"[$t name]">_<"[$comp name]">*2+(Nbactivities_<"[$t name]">_<"[$comp name]">-SelfIncomp_<"[$t name]">_<"[$comp name]">)*Max_<"[$comp name]"><'}'> /* size of the activities part*/
const width_<"[$comp name]">: nat is <'set index 1
foreach t [$comp tasks] {'>width_<"[$t name]">_<"[$comp name]"><'
if {$t!=[lindex [$comp tasks] end]} {'>+<'}}}'><"\n"><'if {[llength $services]!=0} {'>const size_<"[$comp name]">: nat is <'
if {[llength $activities]!=0} {'>width_<"[$comp name]">+<'}'>Nbnonact_<"[$comp name]">+1<"\n"><'}'>
type service_<"[$comp name]"> is union <'foreach s $services {'><"[$s name]"><'
if {[$s kind]=="activity"} {'>_<"[[$s task] name]"><'}'>_<"[$comp name]"> | <'}'>
None_<"[$comp name]"> end /* possible names of services */<'
if {[llength $activities]!=0} {'>

type servicetype_<"[$comp name]"> is union Attribute_<"[$comp name]"> | Function_<"[$comp name]"> | Activity_<"[$comp name]"> end<'}'><"\n">type status_<"[$comp name]"> is union  VOID_<"[$comp name]"> /* inactive */ |START_<"[$comp name]"> /*waiting*/ | RUNNING_<"[$comp name]"> /*active*/ | STOP_<"[$comp name]"> /*to pass to the stop state*/ | STOPPING_<"[$comp name]"> /*stopping*/ | STOPPED_<"[$comp name]"> /*ended after being interrupted */ | ETHER_<"[$comp name]"> end<'
if {[llength $services]!=0} {'><"\n">type dynamicservice_<"[$comp name]"> is record name: service_<"[$comp name]">, current: status_<"[$comp name]">, instance: 0..<'
if {[llength $activities]!=0} {'>Max_<"[$comp name]"><'
} else {'>0<'}'> end /* type of the "turn" shared variables */<'}
if {[llength $services]!=0} {'>

type staticScheduler_<"[$comp name]"> is array size_<"[$comp name]"> of dynamicservice_<"[$comp name]"> /* type of the execution array */<'}
if {[llength $activities]!=0} {'>

type Interrupts_<"[$comp name]"> is queue maxinterrupts_<"[$comp name]"> of service_<"[$comp name]"> /* field "interrupts" of a GenoM service */
type IndexA_<"[$comp name]"> is 0..width_<"[$comp name]">-1<'}
if {[llength $services]!=0} {'>

type Index_<"[$comp name]"> is 0..size_<"[$comp name]">-1<'}'><"\n">type CRName_<"[$comp name]"> is union Kill_<"[$comp name]"><'
if {[llength $activities]!=0} {'> | Abort_<"[$comp name]"><'}'> end
type CR_<"[$comp name]"> is record name: CRName_<"[$comp name]">, who: <'
if {[llength $activities]!=0} {'>IndexA_<"[$comp name]"><'
} else {'>0..0<'}'> end<'
if {$inportscounter > 0} {'><"\n">type CNRname_<"[$comp name]"> is union ConnectPort_<"[$comp name]"> end<"\n">type IndexP_<"[$comp name]"> is 0..<"[expr $inportscounter-1]"><"\n">type IndexExt_<"[$comp name]"> is 0..<"[expr [llength $externalports]-1]"><'
set counter 0
foreach inport $inports {'><"\n">type IndexExt_<"[$comp name]">_<"$counter"> is 0..<"[expr [lindex $counters $counter]-1]"><'
incr counter}'><"\n">type CNR_<"[$comp name]"> is record name: CNRname_<"[$comp name]">, which: IndexP_<"[$comp name]">, toext: IndexExt_<"[$comp name]"> end<"\n">type statePort_<"[$comp name]"> is record connected: bool, extindex: IndexExt_<"[$comp name]"> end<"\n">type PortsArray_<"[$comp name]"> is array <"$inportscounter"> of statePort_<"[$comp name]"><'}'><"\n">type CRreply_<"[$comp name]"> is union DONE_<"[$comp name]"> | NOSUCHACT_<"[$comp name]"> end
<'if {[llength $activities]!=0} {
set index 0
foreach t [$comp tasks] {
if {[llength [$t services]]!=0} {'>
const index_<"[$t name]">_<"[$comp name]">: IndexA_<"[$comp name]"> is <"[lindex $indexes $index]"><'
incr index}}}
if {$inportscounter > 0} {'><"\n\n">/* this module has <"$inportscounter"> input port(s)<"\n"><'
for {set k 1} {$k <= $inportscounter} {incr k} {'>the row <"[expr $k-1]"> of PortConnected_<"[$comp name]"> is dedicated to the port <"[[lindex $inports [expr $k-1]] name]\n"><'}'>
the "extindex" field contains (one of the) following value(s):<'
set limit [lindex $counters 0]
set k 0
set counter 0
foreach inport $inports {'><"\n">for the row <"$counter\n"> (<"[$inport name]">):<' 
while {$k < $limit} {'><"\n\t"><'
if {$counter == 0} {'><"$k"><'
} else {'><"[expr $k - [lindex $counters [expr $counter -1]]]"><'}'> means it is connected to the port <"[[lindex $externalports $k] name]"> of the module  <"[[[lindex $externalports $k] component] name]"><'
incr k}
if {[lindex $inports $counter] != [lindex $inports end]} {
set limit [expr $limit + [lindex $counters [expr $counter + 1]]]}
incr counter}}
if {$inportscounter > 0} {    
'> */<'}'>

/* functions */<'
if {[llength $activities]!=0} {'>


/* is an execution task runnable? */
function running_<"[$comp name]"> (execution: staticScheduler_<"[$comp name]">, index: Index_<"[$comp name]">, widthtask: nat): bool is
var limit: nat
begin
	limit:= index+widthtask;
	while index<limit do
		if not (execution[index].current=STOPPED_<"[$comp name]"> or execution[index].current=ETHER_<"[$comp name]"> or execution[index].current=VOID_<"[$comp name]">) then return true end;
		index:= index+1
	end; return false
end

/* service type */
function servtype_<"[$comp name]"> (serv: service_<"[$comp name]">): servicetype_<"[$comp name]"> is
begin<'
if {[llength $functions]!=0} {'><"\n\t">if (<'
foreach f $functions {'>serv=<"[$f name]">_<"[$comp name]"><'
if {$f == [lindex $functions end]} {'>)<'
} else {'> or <'}}'> then return Function_<"[$comp name]"> end;<'}
if {[llength $attributes]!=0} {'><"\n\t">if (<'
foreach a $attributes {'>serv=<"[$a name]">_<"[$comp name]"><'
if {$a == [lindex $attributes end]} {'>)<'
} else {'> or <'}}'> then return Attribute_<"[$comp name]"> end;<'}'><"\n\t">return Activity_<"[$comp name]"> 
end

/* services that I interrupt */
function Iinterrupt_<"[$comp name]"> (serv: service_<"[$comp name]">): Interrupts_<"[$comp name]"> is
begin
	case serv of<'set first 0
if {[llength $services]!=0} {
foreach s $services {
if {[llength [$s interrupts]]!=0} {'><"\n\t"><'
if {$first!=0} {'>| <'}'>
<"[$s name]"><'if {[$s kind]=="activity"} {'>_<"[[$s task] name]"><'}'>_<"[$comp name]"> -> return {|<'
set first 1
if {[llength [$s interrupts]]==[llength [$comp services]]} {
foreach a $activities {'>
<"[$a name]">_<"[[$a task] name]">_<"[$comp name]"><'if {$a != [lindex $activities end]} {'>,<'}}
} else {
foreach i [$s interrupts] {'>
<"[$i name]">_<"[[$i task] name]">_<"[$comp name]"><'if {$i != [lindex [$s interrupts] end]} {'>,<'}}}'>|} <'}}'><"\n\t"><'
if {$first!=0} {'>| <'}}'> any -> return {||}
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
	if execution[index].current= START_<"[$comp name]"> then return STOPPED_<"[$comp name]"> end;
	if execution[index].current= RUNNING_<"[$comp name]"> then return STOP_<"[$comp name]"> end;
	return execution[index].current
end

/* clear */
function clear_<"[$comp name]"> (execution: staticScheduler_<"[$comp name]">, index: Index_<"[$comp name]">): status_<"[$comp name]"> is
begin
	if (execution[index].current=STOPPED_<"[$comp name]"> or execution[index].current=ETHER_<"[$comp name]">) then return VOID_<"[$comp name]"> end;
	return execution[index].current
end

/* unblock the services line */
function next_<"[$comp name]"> (execution: staticScheduler_<"[$comp name]">, index: Index_<"[$comp name]">, indextask: IndexA_<"[$comp name]">, widthtask: nat): Index_<"[$comp name]"> is
var limit: nat
begin
	limit:= indextask+widthtask;
	while index<limit do 
		if (execution[index].current=RUNNING_<"[$comp name]"> or execution[index].current=STOPPING_<"[$comp name]"> or execution[index].current=STOP_<"[$comp name]">) then 
		   return index
		end;
		index:= index+1 
	end; return $(size_<"[$comp name]">-1)
end<'}
if {[llength $services]!=0} {'>


/* validate or stateless */
function needsvalidate_<"[$comp name]"> (serv: service_<"[$comp name]">): bool is
begin<'
set count 0 
set noval [list] 
foreach s $services {
if {[llength [$s validate]] == 0} {
incr count
lappend noval $s}}
if {$count==0} {'><"\n\t">return true <'
} elseif {$count==[llength $services]} {'><"\n\t">return false <'
} else {'><"\n\t">if (<'
foreach serv $noval {'>serv=<"[$serv name]"><'
if {[$serv kind]=="activity"} {'>_<"[[$serv task] name]"><'}'>_<"[$comp name]"><'
if {$count>1} {'> or <'incr count -1} else {'>)<'} }'>then return false end;<"\n\t">return true <'
}'><"\n">end 

/* look for a free slot. Optimized: we no longer check all the array elements */
function multipleinstances_<"[$comp name]"> (execution: staticScheduler_<"[$comp name]">, temp: service_<"[$comp name]">) : Index_<"[$comp name]"> is
var index: Index_<"[$comp name]"> := 0
begin
	foreach index do 
		if execution[index].name=temp then
			if not (servtype_<"[$comp name]">(temp)=Activity_<"[$comp name]">) then return index end; 
			while execution[index].name=temp do 
				if execution[index].current=VOID_<"[$comp name]"> then return index end;
				index:=index+1
			end;
			return $(size_<"[$comp name]">-1)
		end
	end;
	return $(size_<"[$comp name]">-1) /* never reached, but the compiler complains if omitted */
end<'}'>


/* Processes */

/* client */

process client_<"[$comp name]"> [<'
if {[llength $services]!=0} {'>req_<"[$comp name]">: out service_<"[$comp name]">, <'}'>Creq_<"[$comp name]">: out CR_<"[$comp name]"><'
if {[llength $activities]!=0} {'>, reqimm_<"[$comp name]">: out service_<"[$comp name]">, Creqimm_<"[$comp name]">: out CR_<"[$comp name]"><'}
if {$inportscounter > 0} {'>, CNreq_<"[$comp name]">: out CNR_<"[$comp name]"><'
if {[llength $activities]!=0} {'>, CNreqimm_<"[$comp name]">: out CNR_<"[$comp name]"><'}}'><'
if {[llength $activities]!=0} {'>, intrep_<"[$comp name]">: in Index_<"[$comp name]"><'}
if {[llength $services]!=0} {'>, finalrep_<"[$comp name]">: in sync<'}'>, Creply_<"[$comp name]">: in CRreply_<"[$comp name]">] is
states start, getrep
var <'
if {[llength $activities]!=0} {'>temp: Index_<"[$comp name]">, c: IndexA_<"[$comp name]">, <'}'>tempCrep: CRreply_<"[$comp name]"><'
if {$inportscounter > 0} {
set k 0
while {$k < $inportscounter} {'>, t_<"$k">: IndexExt_<"[$comp name]">_<"$k"><'
incr k}}'>


from start<"\n\t"><'
if {[llength $services]!=0 || $inportscounter > 0} {'>select <'
if {[llength $services]!=0} {
foreach s $services {'><"\n\t"><'
if {$s != [lindex $services 0]} {'>[] <'}'>req_<"[$comp name]">!<"[$s name]"><'
if {[$s kind]=="activity"} {'>_<"[[$s task] name]"><'}'>_<"[$comp name]"><'}
if {[llength $activities]!=0} {
foreach s $services {'><"\n\t">[] reqimm_<"[$comp name]">!<"[$s name]"><'
if {[$s kind]=="activity"} {'>_<"[[$s task] name]"><'}'>_<"[$comp name]"><'}'><"\n\t">[] reqimm_<"[$comp name]">!None_<"[$comp name]"><"\n\t"><'}'>[]<'}'> Creq_<"[$comp name]">!{name=Kill_<"[$comp name]">, who=0}<'
if {[llength $activities]!=0} {'><"\n\t">[] c:= any; Creq_<"[$comp name]">!{name=Abort_<"[$comp name]">, who=c}
	[] Creqimm_<"[$comp name]">!{name=Kill_<"[$comp name]">, who=0}
	[] c:= any; Creqimm_<"[$comp name]">!{name=Abort_<"[$comp name]">, who=c}<'}

if {$inportscounter > 0} {
set k 0
while {$k < $inportscounter} {'><"\n\t">[] t_<"$k">:= any; CNreq_<"[$comp name]">!{name=ConnectPort_<"[$comp name]">, which=<"$k">, toext=t_<"$k">}<'
if {[llength $activities]!=0} {'><"\n\t">[] t_<"$k">:= any; CNreqimm_<"[$comp name]">!{name=ConnectPort_<"[$comp name]">, which=<"$k">, toext=t_<"$k">}<'}
incr k}}'><"\n\t">end; to getrep<'

} else {'> Creq_<"[$comp name]">!{name=Kill_<"[$comp name]">, who=0}; to getrep<'}'>


from getrep<'if {[llength $services]!=0} {'> select <'
if {[llength $activities]!=0} {'><"\n\t\t">intrep_<"[$comp name]">?temp; to getrep
		[]<'}'> Creply_<"[$comp name]">?tempCrep; to getrep
		[] finalrep_<"[$comp name]">; to start
	    end<'
} else {'> Creply_<"[$comp name]">?tempCrep; to getrep<'}'>


/* control task */

process CT_<"[$comp name]"> [<'
if {[llength $services]!=0} {'>req_<"[$comp name]">: in service_<"[$comp name]">, <'}'>Creq_<"[$comp name]">: in CR_<"[$comp name]"><'
if {[llength $activities]!=0} {'>, reqimm_<"[$comp name]">: in service_<"[$comp name]">, Creqimm_<"[$comp name]">: in CR_<"[$comp name]"><'}
if {$inportscounter > 0} {'>, CNreq_<"[$comp name]">: in CNR_<"[$comp name]"><'
if {[llength $activities]!=0} {'>, CNreqimm_<"[$comp name]">: in CNR_<"[$comp name]"><'}}'><'
if {[llength $activities]!=0} {'>, intrep_<"[$comp name]">: out Index_<"[$comp name]"><'}
if {[llength $services]!=0} {'>, finalrep_<"[$comp name]">: sync<'}'>, Creply_<"[$comp name]">: out CRreply_<"[$comp name]">](<'
if {[llength $services]!=0} {'>&execution_<"[$comp name]">: staticScheduler_<"[$comp name]">, <'}'>&lock_<"[$comp name]">:0..<'
if {[llength $activities]!=0} {'>2, <'
} else {'>1, <'}'>&shut_<"[$comp name]">: bool<'
if {[llength $activities]!=0} {'>, &sched_<"[$comp name]">: bool<'}
if {$inportscounter > 0} {'>, &PortConnected_<"[$comp name]">: PortsArray_<"[$comp name]"><'}'>) is
states start,<'
if {[llength $services]!=0} {'> manage, finish,<'}'> manageCR, shutdown<'
if {$inportscounter > 0} {'>, manageCNR<'}'><"\n">var <'
if {[llength $services]!=0} {'>index: Index_<"[$comp name]">, temp: service_<"[$comp name]">, <'}
if {[llength $activities]!=0} {'>index2: IndexA_<"[$comp name]">, line: queue width_<"[$comp name]"> of 0..size_<"[$comp name]">-1, launcher: bool:= true, <'}'>tempCR: CR_<"[$comp name]"><'
if {$inportscounter > 0} {'>, tempCNR: CNR_<"[$comp name]"><'}'>


from start on lock_<"[$comp name]">=0; lock_<"[$comp name]">:=1;<'
if {[llength $services]!=0} {'> index:= 0;<'}
if {[llength $activities]!=0} {'><"\n\t">if sched_<"[$comp name]"> then sched_<"[$comp name]">:= false; 
	select	reqimm_<"[$comp name]">?temp; if temp=None_<"[$comp name]"> then to finish else to manage end
	     [] Creqimm_<"[$comp name]">?tempCR; to manageCR<'
if {$inportscounter > 0} {'><"\n\t\t">[] CNreqimm_<"[$comp name]">?tempCNR; to manageCNR<'}'><"\n\t">end
 	else<'}
if {[llength $services]!=0 || $inportscounter > 0} {'><"\n\t">select <'
if {[llength $services]!=0} {'>req_<"[$comp name]">?temp; if temp=None_<"[$comp name]"> then to finish else to manage end
	     []<'}'> Creq_<"[$comp name]">?tempCR; to manageCR<'
if {$inportscounter > 0} {'><"\n\t\t">[] CNreq_<"[$comp name]">?tempCNR; to manageCNR<'}'><"\n\t">end<'
} else {'>	Creq_<"[$comp name]">?tempCR; to manageCR<'}
if {[llength $activities]!=0} {'><"\n\t">end<'}   
if {[llength $services]!=0} {'>


from manage /* intermediate replies for activity requests */
	index:= multipleinstances_<"[$comp name]">(execution_<"[$comp name]">,temp);
	if index=size_<"[$comp name]">-1 then wait [0,0]; to finish
	else
		if needsvalidate_<"[$comp name]">(temp) then
			select execution_<"[$comp name]">[index].current:= VOID_<"[$comp name]">
	    	    	    [] execution_<"[$comp name]">[index].current:= START_<"[$comp name]"> 
			end
		else
			execution_<"[$comp name]">[index].current:= START_<"[$comp name]">
		end;
		if execution_<"[$comp name]">[index].current= VOID_<"[$comp name]"> then wait[0,0] 
		else<'
if {[llength $activities]!=0} {'><"\n">
    			line:= sitesincomp_<"[$comp name]">(temp, execution_<"[$comp name]">, index);	
			while not (empty line) do
				execution_<"[$comp name]">[first line].current:= manageinterrupt_<"[$comp name]"> (execution_<"[$comp name]">, first line);
				if execution_<"[$comp name]">[first line].current=STOP_<"[$comp name]"> or execution_<"[$comp name]">[first line].current=STOPPING_<"[$comp name]"> then launcher:=false end;
				line:= dequeue line
			end;
			if index<width_<"[$comp name]"> then
				if launcher then execution_<"[$comp name]">[index].current:= RUNNING_<"[$comp name]"> end;
			/* send intermediate reply */
				intrep_<"[$comp name]">!index
			else<'}'>
			/* execute the non-activity here */
			wait [0,0];
			execution_<"[$comp name]">[index].current:= VOID_<"[$comp name]"><'
if {[llength $activities]!=0} {'><"\n\t\t\t">end<'}'><"\n\t\t">end
	end; to finish<'}'>


from manageCR<'
if {[llength $activities]!=0} {'><"\n\t">if tempCR.name=Kill_<"[$comp name]"> then
		foreach index2 do
			execution_<"[$comp name]">[index2].current:= manageinterrupt_<"[$comp name]"> (execution_<"[$comp name]">, index2)
		end;
		Creply_<"[$comp name]">!DONE_<"[$comp name]">; shut_<"[$comp name]">:= true; lock_<"[$comp name]">:= 0; to shutdown
	else 	if execution_<"[$comp name]">[tempCR.who].current=VOID_<"[$comp name]"> then Creply_<"[$comp name]">!NOSUCHACT_<"[$comp name]">
		else execution_<"[$comp name]">[tempCR.who].current:= manageinterrupt_<"[$comp name]"> (execution_<"[$comp name]">, tempCR.who);
		     Creply_<"[$comp name]">!DONE_<"[$comp name]">
		end; to finish
	end<'
} else {'><"\n\t">Creply_<"[$comp name]">!DONE_<"[$comp name]">; shut_<"[$comp name]">:= true; lock_<"[$comp name]">:= 0; to shutdown<'} 
if {$inportscounter > 0} {'>


from manageCNR<"\n\t">PortConnected_<"[$comp name]">[tempCNR.which].connected:=true;<"\n\t">PortConnected_<"[$comp name]">[tempCNR.which].extindex:=tempCNR.toext;<"\n\t">Creply_<"[$comp name]">!DONE_<"[$comp name]">; to <'
if {[llength $activities]!=0} {'>finish<'
} else {'>start<'}'><"\n\n"><'}
if {[llength $services]!=0} {'>


from finish /* final replies */<'
if {[llength $activities]!=0} {'>
	/* process pending activities */
	launcher:= true;
	foreach index2 do
		if execution_<"[$comp name]">[index2].current=START_<"[$comp name]"> then
			line:= sitesincomp_<"[$comp name]">(execution_<"[$comp name]">[index2].name, execution_<"[$comp name]">, index2);
			while not (empty line) do
				execution_<"[$comp name]">[first line].current:= manageinterrupt_<"[$comp name]">(execution_<"[$comp name]">, first line);
				if (execution_<"[$comp name]">[first line].current=STOP_<"[$comp name]"> or execution_<"[$comp name]">[first line].current=STOPPING_<"[$comp name]">) then launcher:= false end; 
			line:= dequeue line
			end;
			if launcher then execution_<"[$comp name]">[index2].current:= RUNNING_<"[$comp name]"> end;
			launcher:= true
		end
	end;	 
	/* update final replies */
	foreach index2 do
		execution_<"[$comp name]">[index2].current:= clear_<"[$comp name]"> (execution_<"[$comp name]">, index2)
	end;<'}'><"\n\t">finalrep_<"[$comp name]">; lock_<"[$comp name]">:= 0; to start<'}
set counterG 0
foreach t [$comp tasks] {
set oneatleastG($counterG) 0
set TS [list]
foreach c [$t codels] {
lappend TS 0}
set counter -1
set oneatleast($counterG) 0
foreach c [$t codels] {
incr counter
set test [thread-safe $comp $c]
if {!$test} {continue}
lset TS $counter 1
if {!$oneatleastG($counterG)} {incr oneatleastG($counterG)}
if {!$oneatleast($counterG)} {incr oneatleast($counterG)}}

set counter -1
set oneatleastS($counterG) [list]
foreach s [$t services] {
lappend oneatleastS($counterG) 0}
foreach s [$t services] {
incr counter
set TSS($counter) [list]
foreach c [$s codels] {
lappend TSS($counter) 0}
set counter2 -1
foreach c [$s codels] {
incr counter2
set test [thread-safe $comp $c]
if {!$test} {continue}
lset TSS($counter) $counter2 1
if {!$oneatleastG($counterG)} {incr oneatleastG($counterG)}
if {![lindex $oneatleastS($counterG) $counter]} {lset oneatleastS($counterG) $counter 1}}}

if {![catch {$t period}]} {'><"\n">
/* task timer */

process timer_<"[$t name]">_<"[$comp name]"> (&tick_<"[$t name]">_<"[$comp name]">: bool, &shuttimer_<"[$t name]">_<"[$comp name]">: bool) is
states start, shutdown

from start
	wait[<"[[$t period] value]">, <"[[$t period] value]">];
	if shuttimer_<"[$t name]">_<"[$comp name]"> then to shutdown end;
	tick_<"[$t name]">_<"[$comp name]">:= true; to start<'}'>


/* task manager */

process Taskmanager_<"[$t name]">_<"[$comp name]"> (&turn_<"[$t name]">_<"[$comp name]"> :dynamicservice_<"[$comp name]"><'
if {[llength [$t services]]!=0} {'>, &execution_<"[$comp name]"> : staticScheduler_<"[$comp name]"><'}
if {![catch {$t period}]} {'>, &tick_<"[$t name]">_<"[$comp name]">: bool, &shuttimer_<"[$t name]">_<"[$comp name]">: bool<'}'>, &lock_<"[$comp name]">:0..<'
if {[llength $activities]!=0} {'>2<'
} else {'>1<'}'>, &shut_<"[$comp name]">: bool<'
if {[llength [$t services]]!=0} {'>, &finished_<"[$t name]">_<"[$comp name]">: bool, &sched_<"[$comp name]">: bool<'}
if {[llength [$t codels]]!=0} {'>, &endperm_<"[$t name]">_<"[$comp name]">: bool<'}
if {$oneatleastG($counterG)} {'>, &async_<"[$t name]">_<"[$comp name]">: bool<'}'>) is
states start, shutdown<'
if {[llength [$t services]]!=0} {'>, orchestrate<"\n">var index: Index_<"[$comp name]">:= 0, firstfiring: bool<'}'>

  
from start<"\n\t"><' 
if {![catch {$t period}]} {'>wait[0,0];<"\n\t"><'}'>on (<'
if {![catch {$t period}]} {'>tick_<"[$t name]">_<"[$comp name]"> and <'}
if {$oneatleastG($counterG)} {'>async_<"[$t name]">_<"[$comp name]"> and <'}'>lock_<"[$comp name]">=0);<'
if {![catch {$t period}]} {'>tick_<"[$t name]">_<"[$comp name]">:= false;<'}'><"\n\t"><'
if {[llength [$t services]]!=0} {'>if not ((running_<"[$comp name]">(execution_<"[$comp name]">, index_<"[$t name]">_<"[$comp name]">, width_<"[$t name]">_<"[$comp name]">))<'
if {[llength [$t codels]]!=0} {'> or not endperm_<"[$t name]">_<"[$comp name]"><'}'>)<'
} else {'>if endperm_<"[$t name]">_<"[$comp name]"><'}'> then 
			if shut_<"[$comp name]"> then<'
if {![catch {$t period}]} {'> shuttimer_<"[$t name]">_<"[$comp name]">:= true;<'}'> to shutdown else to start end // nothing to do. lock implicitely released
	else <'
if {[llength [$t services]]==0} {'>turn_<"[$t name]">_<"[$comp name]">.current:= START_<"[$comp name]">; lock_<"[$comp name]">:= 1; to start<"\n\t">end<'
} else {
if {[llength [$t codels]]!=0} {'><"\n\t\t">if not (running_<"[$comp name]">(execution_<"[$comp name]">, index_<"[$t name]">_<"[$comp name]">, width_<"[$t name]">_<"[$comp name]">)) then<"\n\t\t">turn_<"[$t name]">_<"[$comp name]">:={name=None_<"[$comp name]">, current= START_<"[$comp name]">, instance=0};<"\n\t\t">lock_<"[$comp name]">:= 2; to start<"\n\t\t">else<'}'><"\n\t\t">firstfiring:= true; index:= index_<"[$t name]">_<"[$comp name]">; lock_<"[$comp name]">:=1;<"\n\t\t"><'
if {[llength [$t codels]]!=0} {'>if not endperm_<"[$t name]">_<"[$comp name]"> then<"\n\t\t">turn_<"[$t name]">_<"[$comp name]">:= {name=None_<"[$comp name]">, current= RUNNING_<"[$comp name]">, instance=0};<"\n\t\t">lock_<"[$comp name]">:=2 end;<"\n\t\t">to orchestrate<"\n\t\t">end<'
} else {'><"\n\n\t">to orchestrate<'}'><"\n\t">end

from orchestrate
	wait [0,0]; on <'if {$oneatleastG($counterG)} {'>(async_<"[$t name]">_<"[$comp name]"> and lock_<"[$comp name]">=1);<'
} else {'>lock_<"[$comp name]">=1;<'}'>  
	if not firstfiring then
		if finished_<"[$t name]">_<"[$comp name]"> then
			finished_<"[$t name]">_<"[$comp name]">:= false;
			if sched_<"[$comp name]">=false then sched_<"[$comp name]">:= true end;
			if execution_<"[$comp name]">[index].current=RUNNING_<"[$comp name]"> then execution_<"[$comp name]">[index].current:=ETHER_<"[$comp name]">
			else execution_<"[$comp name]">[index].current:=STOPPED_<"[$comp name]"> end
		end;
		index:= next_<"[$comp name]">(execution_<"[$comp name]">, index+1, index_<"[$t name]">_<"[$comp name]">, width_<"[$t name]">_<"[$comp name]">);
		turn_<"[$t name]">_<"[$comp name]">:= execution_<"[$comp name]">[index];
		if index>width_<"[$comp name]">-1 then lock_<"[$comp name]">:= 0;
			if shut_<"[$comp name]"> then<'
if {![catch {$t period}]} {'> shuttimer_<"[$t name]">_<"[$comp name]">:= true;<'}'> to shutdown else to start end
		else
 		if execution_<"[$comp name]">[index].current=STOP_<"[$comp name]"> then execution_<"[$comp name]">[index].current:=STOPPING_<"[$comp name]"> end;
		lock_<"[$comp name]">:= 2; to orchestrate
		end
	end;
	firstfiring:= false;
	index:= next_<"[$comp name]">(execution_<"[$comp name]">, index, index_<"[$t name]">_<"[$comp name]">, width_<"[$t name]">_<"[$comp name]">);
	turn_<"[$t name]">_<"[$comp name]">:= execution_<"[$comp name]">[index];
	if execution_<"[$comp name]">[index].current=STOP_<"[$comp name]"> then execution_<"[$comp name]">[index].current:=STOPPING_<"[$comp name]"> end;
	lock_<"[$comp name]">:= 2; to orchestrate<'}
if {[llength [$t codels]]!=0} {'><"\n\n">/* task permanent activity automaton */

process Task_<"[$t name]">_<"[$comp name]"> (&lock_<"[$comp name]">: 0..<'
if {[llength $activities]!=0} {'>2, <'
} else {'>1, <'}'>&endperm_<"[$t name]">_<"[$comp name]">: bool, &turn_<"[$t name]">_<"[$comp name]">: dynamicservice_<"[$comp name]"><'if {$oneatleast($counterG)} {'>, &async_<"[$t name]">_<"[$comp name]">:bool<'}'>) is
states <'
set counter 0
foreach c [$t codels] {
foreach tr [$c triggers] {
if {[lindex $TS $counter]} {'><"[$tr name]">_, <"[$tr name]">_2, <"[$tr name]">_3<'
} else {'><"[$tr name]">_<'}
if {$c != [lindex [$t codels] end] || $tr != [lindex [$c triggers] end]} {'>, <'}}
incr counter}
set counter 0
foreach c [$t codels] {
foreach tr [$c triggers] {
if {[lindex $TS $counter]} {'><"\n\n">/* beginning of the asynchronous codel corresponding states */<"\n">from <"[$tr name]">_<"\n\t"><'
if {[llength [$t services]]==0} {'>on (not (turn_<"[$t name]">_<"[$comp name]">.current=VOID_<"[$comp name]">) and lock_<"[$comp name]">=1);<'
} else {'>on (turn_<"[$t name]">_<"[$comp name]">.name=None_<"[$comp name]"> and not (turn_<"[$t name]">_<"[$comp name]">.current=VOID_<"[$comp name]">) and lock_<"[$comp name]">=2);<'}'><"\n\t">async_<"[$t name]">_<"[$comp name]">:=false; lock_<"[$comp name]">:=0;<"\n\t">to <"[$tr name]">_2<"\n">from <"[$tr name]">_2<"\n\t">wait <'
if {![catch {$c wcet}]} {'>[<"[[$c wcet] value]">,<"[[$c wcet] value]">];<'
} else {'>[0,0];<'}'>
/* core of the async codel */ to <"\n\t"> <"[$tr name]">_3<"\n">from <"[$tr name]">_3<"\n\t">on lock_<"[$comp name]">=0; async_<"[$t name]">_<"[$comp name]">:=true;<'
if {[llength [$t services]]==0} {'>lock_<"[$comp name]">:=1;<"\n\t"><'
} else {'>lock_<"[$comp name]">:=2;<'}'><"\n\t"><'
} else {'><"\n\n">from <"[$tr name]">_<"\n\t">wait <'
if {![catch {$c wcet}]} {'>[<"[[$c wcet] value]">,<"[[$c wcet] value]">];<'
} else {'>[0,0];<'}
if {[llength [$t services]]==0} {'>on (not (turn_<"[$t name]">_<"[$comp name]">.current=VOID_<"[$comp name]">) and lock_<"[$comp name]">=1);<"\n\t"><'
} else {'>on (turn_<"[$t name]">_<"[$comp name]">.name=None_<"[$comp name]"> and not (turn_<"[$t name]">_<"[$comp name]">.current=VOID_<"[$comp name]">) and lock_<"[$comp name]">=2);<"\n\t"><'}}
if {[llength [$c yields]]==1} {
if {[[$c yields] name]=="ether"} {'>endperm_<"[$t name]">_<"[$comp name]">:=true;<'
if {[llength [$t services]]==0} {'>turn_<"[$t name]">_<"[$comp name]">.current:=VOID_<"[$comp name]">; lock_<"[$comp name]">=0;<"\n\t"><'
} else {'>if turn_<"[$t name]">_<"[$comp name]">.current=START_<"[$comp name]"> then turn_<"[$t name]">_<"[$comp name]">.current:=VOID_<"[$comp name]">; lock_<"[$comp name]">:=0 else lock_<"[$comp name]">:=1 end;<"\n\t"><'}'>
 to start_<'
} else {
if {[[$c yields] kind] == "pause event"} {
if {[llength [$t services]]==0} {'>turn_<"[$t name]">_<"[$comp name]">.current:=VOID_<"[$comp name]">; lock_<"[$comp name]">=0;<"\n\t"><'
} else {'>if turn_<"[$t name]">_<"[$comp name]">.current=START_<"[$comp name]"> then turn_<"[$t name]">.current:=VOID_<"[$comp name]">; lock_<"[$comp name]">:=0 else lock_<"[$comp name]">:=1 end;<"\n\t"><'}}'>to <"[[$c yields] name]">_<'}
} else {'>select<'
foreach y [$c yields] {'><"\n\t"><'
if {$y!=[lindex [$c yields] 0]} {'>[] <'}
if {[$y name]=="ether"} {'>endperm_<"[$t name]">_<"[$comp name]">:=true;<'
if {[llength [$t services]]==0} {'>turn_<"[$t name]">_<"[$comp name]">.current:=VOID_<"[$comp name]">; lock_<"[$comp name]">=0;<"\n\t"><'
} else {'>if turn_<"[$t name]">_<"[$comp name]">.current=START_<"[$comp name]"> then turn_<"[$t name]">_<"[$comp name]">.current:=VOID_<"[$comp name]">; lock_<"[$comp name]">:=0 else lock_<"[$comp name]">:=1 end;<"\n\t"><'}'> to start_<'
} else {
if {[$y kind] == "pause event"} {
if {[llength [$t services]]==0} {'>turn_<"[$t name]">_<"[$comp name]">.current:=VOID_<"[$comp name]">; lock_<"[$comp name]">=0;<"\n\t"><'
} else {'>if turn_<"[$t name]">_<"[$comp name]">.current=START_<"[$comp name]"> then turn_<"[$t name]">_<"[$comp name]">.current:=VOID_<"[$comp name]">; lock_<"[$comp name]">:=0 else lock_<"[$comp name]">:=1 end;<"\n\t"><'}}'>to <"[$y name]">_<'}}'><"\n\t">end<'}}
incr counter}}'><"\n">

/* services managed by <"[$t name]">_<"[$comp name]"> */
/* if the codel stop is never reached explicitely (in the yields field), then no need to create a state correspoding to it*/

<'
set counter 0
foreach s [$t services] {
set tempcodels [list]
set stopsite -1
set codsite 0'><"\n\n">process <"[$s name]">_<"[[$s task] name]">_<"[$comp name]">(instance: 0..Max_<"[$comp name]">, &turn_<"[[$s task] name]">_<"[$comp name]">: dynamicservice_<"[$comp name]">, &finished_<"[[$s task] name]">_<"[$comp name]">: bool, &lock_<"[$comp name]">:0..2<'if {[lindex $oneatleastS($counterG) $counter]} {'>, &async_<"[$t name]">_<"[$comp name]">:bool<'}'>) is		  
states <'set stopnec 0
set stopasync 0
foreach c [$s codels] {
foreach tr [$c triggers] {
if {[$tr name]=="stop" && [thread-safe $comp $c]} {
set stopnec 1
set stopasync 1
break}}
if {$stopnec} {break}
foreach y [$c yields] {
if {[$y name]=="stop"} {
set stopnec 1
break}}
if {$stopnec} {break}
}
set counter2 0
foreach c [$s codels] {
foreach tr [$c triggers] {
if {[$tr name]=="stop"} {
set stopsite $codsite}
if {[$tr name]!="stop" || $stopnec==1} {
if {[lindex $TSS($counter) $counter2]} {
lappend tempcodels [join [list [$tr name] ""] _] [join [list [$tr name] "2"] _] [join [list [$tr name] "3"] _]
} else {lappend tempcodels [join [list [$tr name] ""] _]}}}
incr codsite
incr counter2}
foreach cod $tempcodels {'><"$cod"><'
if {$cod != [lindex $tempcodels end]} {'>, <'}}'><"\n">var label: service_<"[$comp name]">:= <"[$s name]">_<"[[$s task] name]">_<"[$comp name]"><'
set counter2 0
foreach c [$s codels] {
foreach par [$c parameters] {
if {![catch {$par port}]} {'><"\n\n\t">/* this codel <'
if {[[$par port] dir]=="in"} {'>reads <'
} else {'>writes <'}'>the port <"[[$par port] name]"> */<'}}
foreach tr [$c triggers] {
if {[$tr name]!="stop" || $stopnec==1} {'><"\n\n">from <' 
if {[lindex $TSS($counter) $counter2]} {'><"[$tr name]">_<"\n\t">on (turn_<"[[$s task] name]">_<"[$comp name]">.name = label and turn_<"[[$s task] name]">_<"[$comp name]">.instance=instance and lock_<"[$comp name]">=2);/* beginning of the async codel corresponding states */<"\n\t"><'
if {[$tr name]!="stop"} {'>if turn_<"[[$s task] name]">_<"[$comp name]">.current=STOP_<"[$comp name]"> then <'}
if {$stopsite==-1} {'>wait [0,0]; lock_<"[$comp name]">:=1; finished_<"[[$s task] name]">_<"[$comp name]">:=true; to start_ /* absence of stop codel.. directly to ether */<'
} else {
if {[lindex $TSS($counter) $stopsite]} {'>wait [0,0]; lock_<"[$comp name]">:=0; async_<"[$t name]">_<"[$comp name]">:= false; to stop_2<'
} else {
if {![catch {[lindex [$s codels] $stopsite] wcet}]} {'>wait [<"[[[lindex [$s codels] $stopsite] wcet] value]">,<"[[[lindex [$s codels] $stopsite] wcet] value]">];<'
} else {'>wait [0,0];<'}'><"\n\t"><'
if {[llength [[lindex [$s codels] $stopsite]  yields]]==1} {
if {[[[lindex [$s codels] $stopsite] yields] name]=="ether"} {'>finished_<"[[$s task] name]">_<"[$comp name]">:=true; lock_<"[$comp name]">:=1; to start_ /* execution of the stop codel here */<'
} else {
if {[[[lindex [$s codels] $stopsite]  yields] kind] == "pause event"} {'>
lock_<"[$comp name]">:=1; <'}'>to <"[[[lindex [$s codels] $stopsite]  yields] name]">_<'}
} else {'>select<'
foreach y [[lindex [$s codels] $stopsite] yields] {'><"\n\t"><'
if {$y!=[[lindex [lindex [$s codels] $stopsite] yields] 0]} {'>[] <'}
if {[$y name]=="ether"} {'>finished_<"[[$s task] name]">_<"[$comp name]">:=true; lock_<"[$comp name]">:=1; to start_<'
} else {
if {[$y kind] == "pause event"} {'>
lock_<"[$comp name]">:=1; <'}'>to <"[$y name]">_<'}}'><"\n\t">end<'}}}
if {[$tr name]!="stop"} {'><"\n\t">else wait [0,0]; lock_<"[$comp name]">:=0; async_<"[$t name]">_<"[$comp name]">:= false end; to <"[$tr name]">_2<'}'><"\n\n">from <"[$tr name]">_2 /* core of the codel */ wait <'
if {![catch {$c wcet}]} {'>[<"[[$c wcet] value]">,<"[[$c wcet] value]">];<'
} else {'>[0,0];<'}'> to <"[$tr name]">_3<"\n\n">from <"[$tr name]">_3<"\n\t">wait [0,0]; on lock_<"[$comp name]">=0; lock_<"[$comp name]">:=2; async_<"[$t name]">_<"[$comp name]">:= true;<"\n\t"><'
if {[llength [$c yields]]==1} {
if {[[$c yields] name]=="ether"} {'>finished_<"[[$s task] name]">_<"[$comp name]">:=true; lock_<"[$comp name]">:=1; to start_<'
} else {
if {[[$c yields] kind] == "pause event"} {'>
lock_<"[$comp name]">:=1; <'}'>to <"[[$c yields] name]">_<'}
} else {'>select<'
foreach y [$c yields] {'><"\n\t"><'
if {$y!=[lindex [$c yields] 0]} {'>[] <'}
if {[$y name]=="ether"} {'>finished_<"[[$s task] name]">_<"[$comp name]">:=true; lock_<"[$comp name]">:=1; to start_<'
} else {
if {[$y kind] == "pause event"} {'>
lock_<"[$comp name]">:=1; <'}'>to <"[$y name]">_<'}}'><"\n\t">end<'}'>/* end of the async codel corresponding states */<"\n"><'

} else {'><"[$tr name]">_<"\n\t">on (turn_<"[[$s task] name]">_<"[$comp name]">.name = label and turn_<"[[$s task] name]">_<"[$comp name]">.instance=instance and lock_<"[$comp name]">=2);<"\n\t"><'
if {[$tr name]!="stop"} {'>if turn_<"[[$s task] name]">_<"[$comp name]">.current=STOP_<"[$comp name]"> then <'}
if {$stopsite==-1} {'>wait [0,0]; finished_<"[[$s task] name]">_<"[$comp name]">:=true; lock_<"[$comp name]">:=1; to start_ /* absence of stop codel.. directly to ether */<'
} else {
if {$stopasync} {'>wait [0,0]; lock_<"[$comp name]">:=0; async_<"[$t name]">_<"[$comp name]">:=false; to stop_2<'
} else {
if {![catch {[lindex [$s codels] $stopsite] wcet}]} {'>wait [<"[[[lindex [$s codels] $stopsite] wcet] value]">,<"[[[lindex [$s codels] $stopsite] wcet] value]">];<'
} else {'>wait [0,0];<'}'><"\n\t"><'
if {[llength [[lindex [$s codels] $stopsite]  yields]]==1} {
if {[[[lindex [$s codels] $stopsite] yields] name]=="ether"} {'>finished_<"[[$s task] name]">_<"[$comp name]">:=true; lock_<"[$comp name]">:=1; to start_ /* execution of the stop codel here */<'
} else {
if {[[[lindex [$s codels] $stopsite]  yields] kind] == "pause event"} {'>
lock_<"[$comp name]">:=1; <'}'>to <"[[[lindex [$s codels] $stopsite]  yields] name]">_<'}
} else {'>select<'
foreach y [[lindex [$s codels] $stopsite] yields] {'><"\n\t"><'
if {$y!=[[lindex [lindex [$s codels] $stopsite] yields] 0]} {'>[] <'}
if {[$y name]=="ether"} {'>finished_<"[[$s task] name]">_<"[$comp name]">:=true; lock_<"[$comp name]">:=1; to start_<'
} else {
if {[$y kind] == "pause event"} {'>
lock_<"[$comp name]">:=1; <'}'>to <"[$y name]">_<'}}'><"\n\t">end<'}}}

if {[$tr name]!="stop"} {'><"\n\t">else wait <'
if {![catch {$c wcet}]} {'>[<"[[$c wcet] value]">,<"[[$c wcet] value]">];<'
} else {'>[0,0];<'}'><"\n\t"><'
if {[llength [$c yields]]==1} {
if {[[$c yields] name]=="ether"} {'>finished_<"[[$s task] name]">_<"[$comp name]">:=true; lock_<"[$comp name]">:=1; to start_<'
} else {
if {[[$c yields] kind] == "pause event"} {'>
lock_<"[$comp name]">:=1; <'}'>to <"[[$c yields] name]">_<'}
} else {'>select<'
foreach y [$c yields] {'><"\n\t"><'
if {$y!=[lindex [$c yields] 0]} {'>[] <'}
if {[$y name]=="ether"} {'>finished_<"[[$s task] name]">_<"[$comp name]">:=true; lock_<"[$comp name]">:=1; to start_<'
} else {
if {[$y kind] == "pause event"} {'>
lock_<"[$comp name]">:=1; <'}'>to <"[$y name]">_<'}}'><"\n\t">end<'}'><"\n\t">end<'}}}'>/* end of groupped triggers */<'}
incr counter2}
incr counter}
incr counterG}'>


/* Main component */
	
component <"[$comp name]"> is
var <'
if {[llength $services]!=0} {'>execution_<"[$comp name]">: staticScheduler_<"[$comp name]"> := [<'
foreach a $activities {
set incomp 0
foreach inc $selfincompatible {
if {$a == $inc} {
set incomp 1'>{name=<"[$a name]">_<"[[$a task] name]">_<"[$comp name]">, current=VOID_<"[$comp name]">, instance=1}, {name=<"[$a name]">_<"[[$a task] name]">_<"[$comp name]">, current=VOID_<"[$comp name]">, instance=2}, <'
break}}
if {$incomp==0} {
for {set k 1} {$k <= $max} {incr k} {'>{name=<"[$a name]">_<"[[$a task] name]">_<"[$comp name]">, current=VOID_<"[$comp name]">, instance=<"$k">}, <'}}}
foreach at $attributes {'>
{name=<"[$at name]">_<"[$comp name]">, current=VOID_<"[$comp name]">, instance=0}, <'}
foreach f $functions {'>
{name=<"[$f name]">_<"[$comp name]">, current=VOID_<"[$comp name]">, instance=0}, <'}'>{name=None_<"[$comp name]">, current=VOID_<"[$comp name]">, instance=0}],<'}
set counterG 0
foreach t [$comp tasks] {'> turn_<"[$t name]">_<"[$comp name]">: dynamicservice_<"[$comp name]">:={name=None_<"[$comp name]">, current=VOID_<"[$comp name]">, instance=0},<'
if {[llength [$t services]]!=0} {'> finished_<"[$t name]">_<"[$comp name]">: bool:= false,<'}
if {[llength [$t codels]]!=0} {'> endperm_<"[$t name]">_<"[$comp name]">: bool:= false,<'}
if {![catch {$t period}]} {'> tick_<"[$t name]">_<"[$comp name]">: bool:= false, shuttimer_<"[$t name]">_<"[$comp name]">: bool:= false,<'}
if {$oneatleastG($counterG)} {'> async_<"[$t name]">_<"[$comp name]">: bool:= true,<'}
incr counterG}'> shut_<"[$comp name]">: bool:= false, lock_<"[$comp name]">: 0..<'
if {[llength $activities]!=0} {'>2<'
} else {'>1<'}'>:= 0<'
if {[llength $activities]!=0} {'>, sched_<"[$comp name]">: bool:= false<'}
if {$inportscounter > 0} {'>, PortConnected_<"[$comp name]">: PortsArray_<"[$comp name]">:= [<'
for {set k 1} {$k <= $inportscounter} {incr k} {'>{connected= false, extindex=0}<'
if {$k!=$inportscounter} {'>, <'}}'>]<'}'>

port<"\n\t"><'
if {[llength $activities]!=0} {'>reqimm_<"[$comp name]">: service_<"[$comp name]"> in [0,0],<"\n\t">Creqimm_<"[$comp name]">: CR_<"[$comp name]"> in [0,0],<"\n\t">intrep_<"[$comp name]">: Index_<"[$comp name]"> in [0,0],<"\n\t"><'}
if {[llength $services]!=0} {'>req_<"[$comp name]">: service_<"[$comp name]">,<"\n\t">finalrep_<"[$comp name]">: sync in [0,0],<"\n\t"><'}
if {$inportscounter > 0} {'>CNreq_<"[$comp name]">: CNR_<"[$comp name]">,<"\n\t"><'
if {[llength $activities]!=0} {'>CNreqimm_<"[$comp name]">: CNR_<"[$comp name]"> in [0,0],<"\n\t"><'}}'>Creq_<"[$comp name]">: CR_<"[$comp name]">,<"\n\t">Creply_<"[$comp name]">: CRreply_<"[$comp name]"> in [0,0]
 
par <"\n\t"><'
if {[llength $services]!=0} {'>req_<"[$comp name]">, <'}'>Creq_<"[$comp name]"><'
if {[llength $activities]!=0} {'>, reqimm_<"[$comp name]">, Creqimm_<"[$comp name]"><'}
if {$inportscounter > 0} {'>, CNreq_<"[$comp name]"><'
if {[llength $activities]!=0} {'>, CNreqimm_<"[$comp name]"><'}}'><'
if {[llength $activities]!=0} {'>, intrep_<"[$comp name]"><'}
if {[llength $services]!=0} {'>, finalrep_<"[$comp name]"><'}'>, Creply_<"[$comp name]"> -> client_<"[$comp name]"> [<'
if {[llength $services]!=0} {'>req_<"[$comp name]">, <'}'>Creq_<"[$comp name]"><'
if {[llength $activities]!=0} {'>, reqimm_<"[$comp name]">, Creqimm_<"[$comp name]"><'}
if {$inportscounter > 0} {'>, CNreq_<"[$comp name]"><'
if {[llength $activities]!=0} {'>, CNreqimm_<"[$comp name]"><'}}'><'
if {[llength $activities]!=0} {'>, intrep_<"[$comp name]"><'}
if {[llength $services]!=0} {'>, finalrep_<"[$comp name]"><'}'>, Creply_<"[$comp name]">]<"\n\t">|| <'
if {[llength $services]!=0} {'>req_<"[$comp name]">, <'}'>Creq_<"[$comp name]"><'
if {[llength $activities]!=0} {'>, reqimm_<"[$comp name]">, Creqimm_<"[$comp name]"><'}
if {$inportscounter > 0} {'>, CNreq_<"[$comp name]"><'
if {[llength $activities]!=0} {'>, CNreqimm_<"[$comp name]"><'}}'><'
if {[llength $activities]!=0} {'>, intrep_<"[$comp name]"><'}
if {[llength $services]!=0} {'>, finalrep_<"[$comp name]"><'}'>, Creply_<"[$comp name]"> -> CT_<"[$comp name]"> [<'
if {[llength $services]!=0} {'>req_<"[$comp name]">, <'}'>Creq_<"[$comp name]"><'
if {[llength $activities]!=0} {'>, reqimm_<"[$comp name]">, Creqimm_<"[$comp name]"><'}
if {$inportscounter > 0} {'>, CNreq_<"[$comp name]"><'
if {[llength $activities]!=0} {'>, CNreqimm_<"[$comp name]"><'}}'><'
if {[llength $activities]!=0} {'>, intrep_<"[$comp name]"><'}
if {[llength $services]!=0} {'>, finalrep_<"[$comp name]"><'}'>, Creply_<"[$comp name]">](<'
if {[llength $services]!=0} {'>&execution_<"[$comp name]">, <'}'>&lock_<"[$comp name]">, &shut_<"[$comp name]"><'
if {[llength $activities]!=0} {'>, &sched_<"[$comp name]"><'}
if {$inportscounter > 0} {'>, &PortConnected_<"[$comp name]"><'}'>)<'
set counterG 0
foreach t [$comp tasks] {
if {![catch {$t period}]} {'><"\n\t">|| timer_<"[$t name]">_<"[$comp name]"> (&tick_<"[$t name]">_<"[$comp name]">, &shuttimer_<"[$t name]">_<"[$comp name]">)<'}'><"\n\t">|| Taskmanager_<"[$t name]">_<"[$comp name]"> (&turn_<"[$t name]">_<"[$comp name]"><'
if {[llength [$t services]]!=0} {'>, &execution_<"[$comp name]"><'}
if {![catch {$t period}]} {'>, &tick_<"[$t name]">_<"[$comp name]">, &shuttimer_<"[$t name]">_<"[$comp name]"><'}'>, &lock_<"[$comp name]">, &shut_<"[$comp name]"><'
if {[llength [$t services]]!=0} {'>, &finished_<"[$t name]">_<"[$comp name]">, &sched_<"[$comp name]"><'}
if {[llength [$t codels]]!=0} {'>, &endperm_<"[$t name]">_<"[$comp name]"><'}
if {$oneatleastG($counterG)} {'>, &async_<"[$t name]">_<"[$comp name]"><'}'>)<'
if {[llength [$t codels]]!=0} {'><"\n\t">|| Task_<"[$t name]">_<"[$comp name]"> (&lock_<"[$comp name]">, &endperm_<"[$t name]">_<"[$comp name]">, &turn_<"[$t name]">_<"[$comp name]"><'
if {$oneatleast($counterG)} {'>, &async_<"[$t name]">_<"[$comp name]"><'
}'>)<'}
set counter 0
foreach s [$t services] {
set async 0
if {[lindex $oneatleastS($counterG) $counter]} {
set async 1}
set incomp 0
foreach inc $selfincompatible {
if {$s == $inc} {
set incomp 1'><"\n\t">|| <"[$s name]">_<"[[$s task] name]">_<"[$comp name]"> (1, &turn_<"[[$s task] name]">_<"[$comp name]">, &finished_<"[[$s task] name]">_<"[$comp name]">, &lock_<"[$comp name]"><'
if {$async==1} {'>, &async_<"[$t name]">_<"[$comp name]"><'}'>)<"\n\t">|| <"[$s name]">_<"[[$s task] name]">_<"[$comp name]"> (2, &turn_<"[[$s task] name]">_<"[$comp name]">, &finished_<"[[$s task] name]">_<"[$comp name]">, &lock_<"[$comp name]"><'
if {$async==1} {'>, &async_<"[$t name]">_<"[$comp name]"><'}'>)<'
break}}
if {$incomp==0} {
for {set k 1} {$k <= $max} {incr k} {'><"\n\t">|| <"[$s name]">_<"[[$s task] name]">_<"[$comp name]"> (<"$k">, &turn_<"[[$s task] name]">_<"[$comp name]">, &finished_<"[[$s task] name]">_<"[$comp name]">, &lock_<"[$comp name]"><'if {$async==1} {'>, &async_<"[$t name]">_<"[$comp name]"><'}'>)<'}}
incr counter}
incr counterG}'>

end

<'}'>

/* main component */

component Modules is
par<'
foreach comp [dotgen components] {'><"\n\t"><'
if {$comp != [lindex [dotgen components] 0]} {'>|| <'}'><"[$comp name]"><'}'><"\n">end

/* Entry point for verification */

Modules<'}'>
/*
/* Properties */

/* when shut down, all services have been properly ended */
property safeend is ltl [] (Module/7/state shutdown => not Module/8/value (running execution))
assert safeend

/* a deadlock implies prior reception of a kill request  *TRUE*/
/* this is done via two steps: 
1/ check with selt that a deadlock implies, always, the configuration {timer_shutdown, Task_shutdown, CT_shutdown}
2/ assert that this configuration is never reached unless we generate a Kill request (done here) */

//property ddlcheck is ltl [](not (Module/4/tag ddlcheckhere) => not <> (Module/2/state shutdown)) 
//assert ddlcheck
property ddlcheck is ltl (([] not (Module/7/state shutdown)) or not (not (Module/9/tag ddlcheckhere) until (Module/7/state shutdown))) 
assert ddlcheck

/* control task transitions are never firable when services are running *TRUE*/
property mutualex is ltl [] (Module/8/state orchestrate => not (Module/10/state manage or Module/10/state finish))
assert mutualex */




