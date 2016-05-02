#!/bin/sh
# \
exec tclsh "$0" ${1+"$@"}

package require textutil

proc awkread {} {
    while {[gets stdin line] >= 0} {
        set record [list]
        set line [string trim $line]
        foreach field [::textutil::splitx $line {\s+}] {
            lappend record [list %str $field]
        }
        puts $record
        puts "#"
    }
}

awkread
