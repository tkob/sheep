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
    } elseif {$first == "-translation"} {
        lassign $argv translation
        shift argv 1
    } else {
        puts "unknown option: $first"
        exit 1
    }
}

chan configure stdout -encoding utf-8

proc readIn {f} {
    global encoding
    global translation
    if {[info exists encoding] && [info exists translation]} {
        chan configure $f -encoding ${encoding} -translation ${translation}
    } elseif {[info exists encoding]} {
        chan configure $f -encoding ${encoding}
    } elseif {[info exists translation]} {
        chan configure $f -translation ${translation}
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
