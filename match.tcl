package require Tcl 8.6

namespace eval sheepruntime {
    namespace export match

    proc matchint {pat val} {
        return [expr {$pat == $val}]
    }
    
    proc matchstr {pat val} {
        set tag [lindex $val 0]
        if {$tag != "#str"} { return 0 }
        set str [lindex $val 1]
        return [expr {$pat == $str}]
    }
    
    proc matchvar {pat val} {
        upvar __bindings __bindings
        set __bindings($pat) $val
        return 1
    }
    
    proc matchwild {val} {
        return 1
    }
    
    proc matchvals {pats pidx vals vidx} {
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
        set tag [lindex $val 0]
        if {$tag != "#lst"} { return 0 }
        return [matchvals $pat 0 $val 1]
    }

    proc match {pats vals} {
        puts "pats [llength $pats]: $pats"
        puts "vals [llength $vals]: $vals"
        upvar __bindings __bindings
        return [matchvals $pats 0 $vals 0]
    }
}
