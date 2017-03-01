#!/bin/sh
# \
exec tclsh "$0" ${1+"$@"}

lappend auto_path .

package require messagepack

proc callback {unpacked} {
    puts $unpacked
}

fconfigure stdin -encoding binary

messagepack::unpack_and_callback [list read stdin] callback
