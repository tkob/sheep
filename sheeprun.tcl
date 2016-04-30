#!/bin/sh
# \
exec tclsh "$0" ${1+"$@"}

package require Tcl 8.6
package require cmdline
package require textutil
package require csv

namespace eval sheepruntime {
    namespace export match emitbang

    variable __! {}
    variable __patbody
    variable agenda {}
    variable record {}
    variable infileid
    variable readproc
    variable writeproc
    variable readprocs
    variable writeprocs

    proc debug {msg} {
        if {[info exists ::env(SHEEP_DEBUG)] && $::env(SHEEP_DEBUG)} {
            puts $msg
        }
    }

    proc matchint {pat val} {
        debug "matchint: pat: $pat"
        debug "matchint: val: $val"
        return [expr {$pat == $val}]
    }

    proc matchstr {pat val} {
        debug "matchstr: pat: $pat"
        debug "matchstr: val: $val"
        set tag [lindex $val 0]
        debug "matchstr: tag: $tag"
        if {$tag != "%str"} { return 0 }
        set str [lindex $val 1]
        return [expr {$pat == $str}]
    }

    proc matchvar {pat val} {
        debug "matchvar: pat: $pat"
        debug "matchvar: val: $val"
        upvar __bindings __bindings
        set __bindings($pat) $val
        return 1
    }

    proc matchwild {val} {
        debug "matchwild: val: $val"
        return 1
    }

    proc matchvals {pats pidx vals vidx} {
        debug "matchvals: pats [llength $pats]: $pats"
        debug "matchvals: pidx: $pidx"
        debug "matchvals: vals [llength $vals]: $vals"
        debug "matchvals: vidx: $vidx"
        upvar __bindings __bindings
        set patslen [llength $pats]
        set valslen [llength $vals]
        if {$pidx == $patslen} {
            # exhausted patterns, no match if values remain
            return [expr {$vidx == $valslen}]
        }
        set patscar [lindex $pats $pidx]
        # dots matches everything including zero-length values 
        if {$patscar == "matchdots"} {
            return 1
        }
        # exhaust values?
        if {!($vidx < $valslen)} {
            return 0
        }
        set valscar [lindex $vals $vidx]
        if {![{*}$patscar $valscar]} {
            return 0
        }
        incr pidx
        incr vidx
        tailcall matchvals $pats $pidx $vals $vidx
    }

    proc matchlist {pat val} {
        debug "matchlist: pat: $pat"
        debug "matchlist: val: $val"
        upvar __bindings __bindings
        set tag [lindex $val 0]
        debug "matchlist: tag: $tag"
        if {$tag != "%lst"} { return 0 }
        return [matchvals $pat 0 $val 1]
    }

    proc match {pats vals} {
        debug "match: pats [llength $pats]: $pats"
        debug "match: vals [llength $vals]: $vals"
        upvar __bindings __bindings
        return [matchvals $pats 0 $vals 0]
    }

    proc emitbang {} {
        variable __!
        variable writeproc
        foreach line ${__!} {
            $writeproc $line
            puts ""
        }
        set __! [list]
    }

    proc read {} {
        variable agenda
        variable record
        variable readproc
        if {[llength $agenda]} {
            set record [lindex $agenda 0]
            set agenda [lrange $agenda 1 end]
            return 1
        } else {
            $readproc
        }
    }

    proc awkread {} {
        variable record
        variable infileid
        if {[gets $infileid line] >= 0} {
            set line [string trim $line]
            foreach field [::textutil::splitx $line {\s+}] {
                lappend record [list %str $field]
            }
            debug $record
            return 1
        } else {
            return 0
        }
    }

    proc awkwrite {record} {
        foreach field $record {
            set tag [lindex $field 0]
            if {$tag == "%cls"} {
                puts -nonewline "[lindex $field 1] "
            } elseif {$tag == "%lst"} {
                awkwrite [lrange $field 1 end]
            } elseif {$tag == "%str"} {
                puts -nonewline "[lindex $field 1] "
            } else {
                puts -nonewline "$field "
            }
        }
    }

    proc flatappend {var record} {
        upvar $var l
        foreach field $record {
            set tag [lindex $field 0]
            if {$tag == "%cls"} {
                lappend l [lindex $field 1]
            } elseif {$tag == "%lst"} {
                flatappend l [lrange $field 1 end]
            } elseif {$tag == "%str"} {
                lappend l [lindex $field 1]
            } else {
                lappend l $field
            }
        }
    }

    proc csvwrite {record} {
        set l [list]
        flatappend l $record
        puts -nonewline [::csv::join $l]
    }

    array set readprocs {
        awk awkread
    }

    array set writeprocs {
        awk awkwrite
        csv csvwrite
    }

    proc getoptions {argvVar} {
        variable readprocs
        variable writeprocs
        variable readproc
        variable writeproc
        upvar argv $argvVar
        set options {
            {f.arg "awk" "input format"}
            {t.arg "awk" "output format"}
        }
        set usage ": \[options]"
        array set params [::cmdline::getoptions argv $options $usage]

        if {![info exists readprocs($params(f))]} {
            error "invalid input format: $params(f)"
        }
        set readproc $readprocs($params(f))

        if {![info exists writeprocs($params(t))]} {
            error "invalid output format: $params(t)"
        }
        set writeproc $writeprocs($params(t))
    }
}

::sheepruntime::getoptions argv

set ::sheepruntime::infileid stdin

puts [lindex $argv 0]
set ::sheepruntime::f [open [lindex $argv 0]]
eval [read $::sheepruntime::f]
close $::sheepruntime::f

if {[llength [info procs __BEGIN]]} {
    __BEGIN
    ::sheepruntime::emitbang
}

set ::sheepruntime::__mode __default

while {[info exists sheepruntime::__patbody($::sheepruntime::__mode)]} {
    ::sheepruntime::debug "mode=${::sheepruntime::__mode}"
    set ::sheepruntime::__! {}
    set ::sheepruntime::record {}
    if {[::sheepruntime::read]} {
        foreach __pb $::sheepruntime::__patbody($::sheepruntime::__mode) {
            if {[${__pb} {*}$::sheepruntime::record]} {
                ::sheepruntime::emitbang
                break
            }
        }
    } else {
        break
    }
}

if {[llength [info procs __END]]} {
    __END
    ::sheepruntime::emitbang
}
