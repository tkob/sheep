#!/bin/sh
# \
exec tclsh "$0" ${1+"$@"}

package require textutil

proc awkread {f} {
    while {[gets $f line] >= 0} {
        set record [list]
        set line [string trim $line]
        foreach field [::textutil::splitx $line {\s+}] {
            if {[string is entier $field]} {
                lappend record $field
            } elseif {[string is double $field]} {
                lappend record $field
            } elseif {[regexp {^(TRUE|[Tt]rue)$} $field]} {
                lappend record true
            } elseif {[regexp {^(FALSE|[Ff]alse)$} $field]} {
                lappend record false
            } else {
                lappend record [list %str $field]
            }
        }
        puts $record
    }
}

fconfigure stdout -encoding utf-8

if {[llength $argv] > 0} {
    foreach filename $argv {
        set f [open $filename]
        awkread $f
        close $f
    }
} else {
    awkread stdin
}
