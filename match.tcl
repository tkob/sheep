package require Tcl 8.6

namespace eval sheepruntime {
    namespace export match

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
            return [expr {$vidx == $valslen}]
        }
        if {!($vidx < $valslen)} {
            return 0
        }
        set patscar [lindex $pats $pidx]
        set valscar [lindex $vals $vidx]
        if {$patscar == "matchdots"} {
            return 1
        }
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
}
