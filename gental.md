# Code Genarator for Tcl Assembly Language #

## Tcl Data representation ##

This section explains how Sheep values are represented in the Tcl runtime.
The rationale is that type of a Sheep value should be reified at runtime, but
the Tcl runtime does not distinguish types of values very well. (because of the
EIAS principle)

### Integer ###

Integer values are represented as is.

### Boolean ###

Boolean values true and false are represented as true and false respectively.

These values can be iterpretted directly by Tcl runtime and are yet
distinguishable from values of other types.
Note that since Tcl uses 1 and 0 for its canonical representation of true and
false respectively, conversion from 1/0 to t/f is required but not vice versa.

### String ###

A string value is represented as a 2-element list whose first element is `%str`
and the second is the string.

### List ###

A list is represented as a list whose first element is `%lst` and the rest is
the list.

### Closure ###

A closure is represented as a 3-elemtent list whose first element is `%cls`,
the second is the function name and the third is a list of actual arguments for
free variables.

## Pattern and Body ##

Pattern-and-bodies that are matched against input are compiled as Tcl procs.
Those procs are grouped by label. When an input record is read, the driver
runtime calls procs with current mode(=label) one by one. If the input does
not match, the proc returns 0. Otherwise, it returns new mode and output.
