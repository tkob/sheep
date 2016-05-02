#!/bin/sh
# \
exec tclsh "$0" ${1+"$@"}

package require textutil

proc awkread {f} {
    while {[gets $f line] >= 0} {
        set record [list]
        set line [string trim $line]
        foreach field [::textutil::splitx $line {\s+}] {
            lappend record [list %str $field]
        }
        puts $record
        puts "#"
    }
}

if {[llength $argv] > 0} {
    foreach filename $argv {
        set f [open $filename]
        awkread $f
        close $f
    }
} else {
    awkread stdin
}
