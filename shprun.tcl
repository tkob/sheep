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

    proc gcd {a b} {
        if {$b == 0} {
            return $a
        } else {
            tailcall gcd $b [expr {$a % $b}]
        }
    }

    # precondition: numer and denom should be integer
    # postcondition: returns rational
    proc rat {numer denom} {
        if {$denom == 0} { return expr {$numer/$denom} }

        if {$denom < 0} {
            set numer [expr {-$numer}]
            set denom [expr {-$denom}]
        }

        set gcd [gcd [expr {abs($numer)}] $denom]
        return [list %rat [expr {$numer / $gcd}] [expr {$denom / $gcd}]]
    }

    # precondition: v should be rational
    proc todouble {v} {
        return [expr {double([lindex $v 1]) / double([lindex $v 2])}]
    }

    # precondition: a and b should be rational
    # postcondition: returns rational or integer
    proc ratadd {a b} {
        lassign $a _ numer_a denom_a
        lassign $b _ numer_b denom_b
        set rat [rat [expr {$numer_a * $denom_b + $numer_b * $denom_a}] \
                     [expr {$denom_a * $denom_b}]]
        set denom_result [lindex $rat 2]
        if {$denom_result == 1} {
            return [lindex $rat 1]
        } else {
            return $rat
        }
    }

    # precondition: a and b should be rational
    # postcondition: returns rational or integer
    proc ratsub {a b} {
        lassign $b _ numer denom
        return [ratadd $a [list %rat [expr {-$numer}] $denom]]
    }

    # precondition: a and b should be rational
    # postcondition: returns rational or integer
    proc ratmul {a b} {
        lassign $a _ numer_a denom_a
        lassign $b _ numer_b denom_b
        set rat [rat [expr {$numer_a * $numer_b}] [expr {$denom_a * $denom_b}]]
        set denom_result [lindex $rat 2]
        if {$denom_result == 1} {
            return [lindex $rat 1]
        } else {
            return $rat
        }
    }

    # precondition: a and b should be rational
    # postcondition: returns rational or integer
    proc ratdiv {a b} {
        lassign $b _ numer denom
        return [ratmul $a [list %rat $denom $numer]]
    }

    proc ratcompare {op a b} {
        lassign $a _ numer_a denom_a
        lassign $b _ numer_b denom_b
        set diff [expr {$numer_a * $denom_b - $numer_b * $denom_a}]
        return [$op $diff 0]
    }

    proc ratgt {a b} {
        return [ratcompare ::tcl::mathop::> $a $b]
    }

    proc ratlt {a b} {
        return [ratcompare ::tcl::mathop::< $a $b]
    }

    proc ratge {a b} {
        return [ratcompare ::tcl::mathop::>= $a $b]
    }

    proc ratle {a b} {
        return [ratcompare ::tcl::mathop::<= $a $b]
    }

    proc arith {op a b} {
        # | a     | b     || proc   | conversion
        # | ----- | ----- || ------ | ----------
        # | int   | int   || expr   |
        # |       | float || expr   |
        # |       | rat   || ratop  | int->rat
        # |       | other || error  |
        # | float | int   || expr   |
        # |       | float || expr   |
        # |       | rat   || expr   | rat->float
        # |       | other || error  |
        # | rat   | int   || ratop  | int->rat
        # |       | float || expr   | rat->float
        # |       | rat   || ratop  |
        # |       | other || error  |
        # | other | -     || error  |
        if {[string is entier $a]} {
            if {[string is double $b]} {
                # if $b is float or int
                return [[lindex $op 0] $a $b]
            } elseif {[lindex $b 0] == "%rat"} {
                return [[lindex $op 1] [rat $a 1] $b]
            } else {
                error "can't use non-numeric value as operand of arith: $b"
            }
        } elseif {[string is double $a]} {
            if {[string is double $b]} {
                # if $b is float or int
                return [[lindex $op 0] $a $b]
            } elseif {[lindex $b 0] == "%rat"} {
                return [[lindex $op 0] $a [todouble $b]]
            } else {
                error "can't use non-numeric value as operand of arith: $b"
            }
        } elseif {[lindex $a 0] == "%rat"} {
            if {[string is entier $b]} {
                return [[lindex $op 1] $a [rat $b 1]]
            } elseif {[string is double $b]} {
                return [[lindex $op 0] [todouble $a] $b]
            } elseif {[lindex $b 0] == "%rat"} {
                return [[lindex $op 1] $a $b]
            } else {
                error "can't use non-numeric value as operand of arith: $b"
            }
        } else {
            error "can't use non-numeric value as operand of arith: $a"
        }
    }

    proc add {a b} {
        return [arith {::tcl::mathop::+ ratadd} $a $b]
    }

    proc sub {a b} {
        return [arith {::tcl::mathop::- ratsub} $a $b]
    }

    proc mul {a b} {
        return [arith {::tcl::mathop::* ratmul} $a $b]
    }

    proc div {a b} {
        if {[string is entier $a] && [string is entier $b]} {
            set rat [rat $a $b]
            set denom_result [lindex $rat 2]
            if {$denom_result == 1} {
                return [lindex $rat 1]
            } else {
                return $rat
            }
        } else {
            return [arith {::tcl::mathop::/ ratdiv} $a $b]
        }
    }

    proc eq {a b} {
        foreach ab [list [list $a $b] [list $b $a]] {
            lassign $ab this that
            if {[lindex $this 0] == "%rat"} {
                if {[string is entier $that]} {
                    return 0
                } elseif {[string is double $that]} {
                    return [arith {::tcl::mathop::== ::tcl::mathop::==} $a $b]
                }
            }
        }
        return [expr {$a == $b}]
    }

    proc gt {a b} {
        return [arith {::tcl::mathop::> ratgt} $a $b]
    }

    proc lt {a b} {
        return [arith {::tcl::mathop::< ratlt} $a $b]
    }

    proc ge {a b} {
        return [arith {::tcl::mathop::>= ratge} $a $b]
    }

    proc le {a b} {
        return [arith {::tcl::mathop::<= ratle} $a $b]
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
            } elseif {$tag == "%rat"} {
                puts -nonewline "[lindex $field 1]/[lindex $field 2] "
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
            } elseif {$tag == "%rat"} {
                lappend l [todouble $field]
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

        source [lindex $argv 0]

        global NR
        set NR 0

        if {[llength [info procs __BEGIN]]} {
            __BEGIN
            ::sheepruntime::emitbang
        }

        set ::sheepruntime::__mode __default

        # If there are no pattern-bodies, input files are not read at all
        if {[array size ::sheepruntime::__patbody] > 0 ||
            [llength [info procs __END]] > 0} {
            set f [open "|shpfront.tcl [lrange $argv 1 end]"]
            set interp [interp create -safe]
            while {1} {
                ::sheepruntime::debug "mode=${::sheepruntime::__mode}"
                set ::sheepruntime::__! {}

                set input ""
                while {[gets $f line] >= 0} {
                    ::sheepruntime::debug "line=$line"
                    if {$line == "#"} { break }
                    set input "$input$line\n"
                }
                if {[eof $f]} { break }

                ::sheepruntime::debug "input=$input"

                set record [interp eval $interp set record "{$input}"]
                ::sheepruntime::debug "record=$record"
                incr NR

                if {[array names ::sheepruntime::__patbody -exact $::sheepruntime::__mode] != {}} {
                    foreach __pb $::sheepruntime::__patbody($::sheepruntime::__mode) {
                        if {[${__pb} {*}$record]} {
                            ::sheepruntime::emitbang
                            break
                        }
                    }
                }
            }
            close $f
        }


        if {[llength [info procs __END]]} {
            __END
            ::sheepruntime::emitbang
        }
    }
}

if {!$tcl_interactive} {
    ::sheepruntime::main
}
