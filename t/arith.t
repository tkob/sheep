# Setup

    % source shprun.tcl

# Addition

    % ::sheepruntime::add 1 2
    3

    % ::sheepruntime::add 1 2.0
    3.0

    % ::sheepruntime::add 1 {%rat 2 3}
    %rat 5 3

    % ::sheepruntime::add 1.0 2
    3.0

    % ::sheepruntime::add 1.0 2.0
    3.0

    % ::sheepruntime::add 1.0 {%rat 1 2}
    1.5

    % ::sheepruntime::add {%rat 1 2} 1
    %rat 3 2

    % ::sheepruntime::add {%rat 1 2} 1.0
    1.5

    % ::sheepruntime::add {%rat 1 2} {%rat 1 2}
    1

    % ::sheepruntime::add {%rat 1 2} {%rat 2 3}
    %rat 7 6

# Subtraction

    % ::sheepruntime::sub 1 2
    -1

    % ::sheepruntime::sub 1 2.0
    -1.0

    % ::sheepruntime::sub 1 {%rat 2 3}
    %rat 1 3

    % ::sheepruntime::sub 1.0 2
    -1.0

    % ::sheepruntime::sub 1.0 2.0
    -1.0

    % ::sheepruntime::sub 1.0 {%rat 1 2}
    0.5

    % ::sheepruntime::sub {%rat 1 2} 1
    %rat -1 2

    % ::sheepruntime::sub {%rat 1 2} 1.0
    -0.5

    % ::sheepruntime::sub {%rat 1 2} {%rat 1 2}
    0

    % ::sheepruntime::sub {%rat 1 2} {%rat 2 3}
    %rat -1 6

# Multiplication

    % ::sheepruntime::mul 2 3
    6

    % ::sheepruntime::mul 2 3.0
    6.0

    % ::sheepruntime::mul 3 {%rat 2 3}
    2

    % ::sheepruntime::mul 2 {%rat 2 3}
    %rat 4 3

    % ::sheepruntime::mul 2.0 3
    6.0

    % ::sheepruntime::mul 2.0 3.0
    6.0

    % ::sheepruntime::mul 2.0 {%rat 1 2}
    1.0

    % ::sheepruntime::mul {%rat 2 3} 2
    %rat 4 3

    % ::sheepruntime::mul {%rat 1 2} 2.0
    1.0

    % ::sheepruntime::mul {%rat 1 2} {%rat 2 1}
    1

    % ::sheepruntime::mul {%rat 1 2} {%rat 1 2}
    %rat 1 4

# Division

    % ::sheepruntime::div 2 3
    %rat 2 3

    % ::sheepruntime::div 1 2.0
    0.5

    % ::sheepruntime::div 2 {%rat 2 3}
    3

    % ::sheepruntime::div 2 {%rat 3 4}
    %rat 8 3

    % ::sheepruntime::div 1.0 2
    0.5

    % ::sheepruntime::div 1.0 2.0
    0.5

    % ::sheepruntime::div 2.0 {%rat 1 2}
    4.0

    % ::sheepruntime::div {%rat 2 3} 2
    %rat 1 3

    % ::sheepruntime::div {%rat 1 2} 2.0
    0.25

    % ::sheepruntime::div {%rat 1 2} {%rat 1 2}
    1

    % ::sheepruntime::div {%rat 1 2} {%rat 2 1}
    %rat 1 4

# Equality

    % ::sheepruntime::eq 2 2
    1

    % ::sheepruntime::eq 2 3
    0

    % ::sheepruntime::eq 2 2.0
    1

    % ::sheepruntime::eq 2 3.0
    0

    % ::sheepruntime::eq 2 {%rat 2 3}
    0

    % ::sheepruntime::eq 1.0 1
    1

    % ::sheepruntime::eq 1.0 2
    0

    % ::sheepruntime::eq 1.0 1.0
    1

    % ::sheepruntime::eq 1.0 2.0
    0

    % ::sheepruntime::eq 0.5 {%rat 1 2}
    1

    % ::sheepruntime::eq 0.5 {%rat 1 3}
    0

    % ::sheepruntime::eq {%rat 2 3} 2
    0

    % ::sheepruntime::eq {%rat 1 2} 0.5
    1

    % ::sheepruntime::eq {%rat 1 2} 2.0
    0

    % ::sheepruntime::eq {%rat 1 2} {%rat 1 2}
    1

    % ::sheepruntime::eq {%rat 1 2} {%rat 1 3}
    0

# Greater Than

    % ::sheepruntime::gt 3 2
    1

    % ::sheepruntime::gt 2 2
    0

    % ::sheepruntime::gt 3 2.0
    1

    % ::sheepruntime::gt 2 2.0
    0

    % ::sheepruntime::gt 1 {%rat 1 2}
    1

    % ::sheepruntime::gt 0 {%rat 1 2}
    0

    % ::sheepruntime::gt 1.1 1
    1

    % ::sheepruntime::gt 1.0 1
    0

    % ::sheepruntime::gt 1.1 1.0
    1

    % ::sheepruntime::gt 1.0 1.0
    0

    % ::sheepruntime::gt 0.5 {%rat 1 3}
    1

    % ::sheepruntime::gt 0.5 {%rat 1 2}
    0

    % ::sheepruntime::gt {%rat 2 3} 0.5
    1

    % ::sheepruntime::gt {%rat 1 2} 0.5
    0

    % ::sheepruntime::gt {%rat 3 2} 1
    1

    % ::sheepruntime::gt {%rat 1 2} 1
    0

    % ::sheepruntime::gt {%rat 2 3} 0.5
    1

    % ::sheepruntime::gt {%rat 1 2} 0.5
    0

    % ::sheepruntime::gt {%rat 3 3} {%rat 2 3}
    1

    % ::sheepruntime::gt {%rat 1 3} {%rat 1 3}
    0

# Less Than

    % ::sheepruntime::lt 2 3
    1

    % ::sheepruntime::lt 2 2
    0

    % ::sheepruntime::lt 2 3.0
    1

    % ::sheepruntime::lt 2 2.0
    0

    % ::sheepruntime::lt 1 {%rat 3 2}
    1

    % ::sheepruntime::lt 1 {%rat 1 2}
    0

    % ::sheepruntime::lt 0.9 1
    1

    % ::sheepruntime::lt 1.0 1
    0

    % ::sheepruntime::lt 0.9 1.0
    1

    % ::sheepruntime::lt 1.0 1.0
    0

    % ::sheepruntime::lt 0.5 {%rat 2 3}
    1

    % ::sheepruntime::lt 0.5 {%rat 1 2}
    0

    % ::sheepruntime::lt {%rat 1 3} 0.5
    1

    % ::sheepruntime::lt {%rat 1 2} 0.5
    0

    % ::sheepruntime::lt {%rat 1 2} 1
    1

    % ::sheepruntime::lt {%rat 3 2} 1
    0

    % ::sheepruntime::lt {%rat 1 3} 0.5
    1

    % ::sheepruntime::lt {%rat 1 2} 0.5
    0

    % ::sheepruntime::lt {%rat 1 3} {%rat 2 3}
    1

    % ::sheepruntime::lt {%rat 1 3} {%rat 1 3}
    0

# Greater Than Or Equal

    % ::sheepruntime::ge 2 3
    0

    % ::sheepruntime::ge 2 2
    1

    % ::sheepruntime::ge 2 3.0
    0

    % ::sheepruntime::ge 2 2.0
    1

    % ::sheepruntime::ge 1 {%rat 3 2}
    0

    % ::sheepruntime::ge 1 {%rat 1 2}
    1

    % ::sheepruntime::ge 0.9 1
    0

    % ::sheepruntime::ge 1.0 1
    1

    % ::sheepruntime::ge 0.9 1.0
    0

    % ::sheepruntime::ge 1.0 1.0
    1

    % ::sheepruntime::ge 0.5 {%rat 2 3}
    0

    % ::sheepruntime::ge 0.5 {%rat 1 2}
    1

    % ::sheepruntime::ge {%rat 1 3} 0.5
    0

    % ::sheepruntime::ge {%rat 1 2} 0.5
    1

    % ::sheepruntime::ge {%rat 1 2} 1
    0

    % ::sheepruntime::ge {%rat 3 2} 1
    1

    % ::sheepruntime::ge {%rat 1 3} 0.5
    0

    % ::sheepruntime::ge {%rat 1 2} 0.5
    1

    % ::sheepruntime::ge {%rat 1 3} {%rat 2 3}
    0

    % ::sheepruntime::ge {%rat 1 3} {%rat 1 3}
    1

# Less Than Or Equal

    % ::sheepruntime::le 3 2
    0

    % ::sheepruntime::le 2 2
    1

    % ::sheepruntime::le 3 2.0
    0

    % ::sheepruntime::le 2 2.0
    1

    % ::sheepruntime::le 1 {%rat 1 2}
    0

    % ::sheepruntime::le 0 {%rat 1 2}
    1

    % ::sheepruntime::le 1.1 1
    0

    % ::sheepruntime::le 1.0 1
    1

    % ::sheepruntime::le 1.1 1.0
    0

    % ::sheepruntime::le 1.0 1.0
    1

    % ::sheepruntime::le 0.5 {%rat 1 3}
    0

    % ::sheepruntime::le 0.5 {%rat 1 2}
    1

    % ::sheepruntime::le {%rat 2 3} 0.5
    0

    % ::sheepruntime::le {%rat 1 2} 0.5
    1

    % ::sheepruntime::le {%rat 3 2} 1
    0

    % ::sheepruntime::le {%rat 1 2} 1
    1

    % ::sheepruntime::le {%rat 2 3} 0.5
    0

    % ::sheepruntime::le {%rat 1 2} 0.5
    1

    % ::sheepruntime::le {%rat 3 3} {%rat 2 3}
    0

    % ::sheepruntime::le {%rat 1 3} {%rat 1 3}
    1

