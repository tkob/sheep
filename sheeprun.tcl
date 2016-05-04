#!/bin/sh
# \
exec tclsh "$0" ${1+"$@"}

package require Tcl 8.6
package require cmdline
package require textutil
package require csv

namespace eval sheepruntime {
    namespace export main

    variable __! {}
    variable __patbody
    variable agenda {}
    variable writeproc
    variable writeprocs

    proc debug {msg} {
        if {[info exists ::env(SHEEP_DEBUG)] && $::env(SHEEP_DEBUG)} {
            puts $msg
        }
    }

    proc app {fun as} {
        $fun {*}$as
    }

    proc appfv {fun fv as} {
        $fun $fv {*}$as
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

    # Matches vals against pats and returns 1 if they match, 0 otherwise.
    # Caller can read __bindings upvar to read matched variables.
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

    array set writeprocs {
        awk awkwrite
        csv csvwrite
    }

    proc getoptions {argvVar} {
        variable writeprocs
        variable writeproc
        upvar argv $argvVar
        set options {
            {t.arg "awk" "output format"}
        }
        set usage ": \[options]"
        array set params [::cmdline::getoptions argv $options $usage]

        if {![info exists writeprocs($params(t))]} {
            error "invalid output format: $params(t)"
        }
        set writeproc $writeprocs($params(t))
    }

    proc main {} {
        global argv
        ::sheepruntime::getoptions argv

        set interp [interp create -safe]

        set f [open "|sheepfront.tcl [lrange $argv 1 end]"]

        source [lindex $argv 0]

        if {[llength [info procs __BEGIN]]} {
            __BEGIN
            ::sheepruntime::emitbang
        }

        set ::sheepruntime::__mode __default

        while {[info exists sheepruntime::__patbody($::sheepruntime::__mode)]} {
            if {[eof $f]} { break }

            ::sheepruntime::debug "mode=${::sheepruntime::__mode}"
            set ::sheepruntime::__! {}

            set input ""
            while {[gets $f line] >= 0} {
                ::sheepruntime::debug "line=$line"
                if {$line == "#"} { break }
                set input "$input$line\n"
            }
            ::sheepruntime::debug "input=$input"

            set record [interp eval $interp set record "{$input}"]
            ::sheepruntime::debug "record=$record"

            foreach __pb $::sheepruntime::__patbody($::sheepruntime::__mode) {
                if {[${__pb} {*}$record]} {
                    ::sheepruntime::emitbang
                    break
                }
            }
        }

        if {[llength [info procs __END]]} {
            __END
            ::sheepruntime::emitbang
        }
    }
}

::sheepruntime::main
