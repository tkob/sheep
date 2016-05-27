#!/bin/sh
# vim: set filetype=tcl : \
exec tclsh "$0" ${1+"$@"}

package require Tcl 8.5

proc shift {var n} {
    upvar $var list
    set list [lrange $list $n end]
}

while {true} {
    lassign $argv first
    if {![string match {-*} $first]} break
    shift argv 1
    if {$first == "--"} {
        break
    } elseif {$first == "-encoding"} {
        lassign $argv encoding
        shift argv 1
    } else {
        puts "unknown option: $first"
        exit 1
    }
}

chan configure stdout -encoding utf-8

proc readIn {f} {
    global encoding
    if {[info exists encoding]} {
        chan configure $f -encoding ${encoding}
    }
    chan copy $f stdout
}

if {[llength $argv] > 0} {
    foreach filename $argv {
        set f [open $filename]
        readIn $f
        close $f
    }
} else {
    readIn stdin
}
