# from UTF-8

    % exec env LANG=en_US.UTF-8 shpfront t/fixture/utf-8.txt | t/unpack.tcl
    %lst {%str 羊}

# from Shift_JIS

    % exec shpfront -opts encoding=shiftjis t/fixture/shiftjis.txt | t/unpack.tcl
    %lst {%str 羊}

# from stdin

    % exec shpfront -opts encoding=utf-8 < t/fixture/utf-8.txt | t/unpack.tcl
    %lst {%str 羊}

# multiple files

    % exec shpfront -opts encoding=utf-8 t/fixture/utf-8.txt t/fixture/utf-8.txt | t/unpack.tcl
    %lst {%str 羊}
    %lst {%str 羊}

# Specify format explicitly

    % exec shpfront -opts awk:encoding=utf-8 t/fixture/utf-8.txt | t/unpack.tcl
    %lst {%str 羊}

# Unknown format

    % exec shpfront -opts unknown:encoding=utf-8 t/fixture/utf-8.txt
    unhandled exception: Fail: unknown format: unknown

# Unknown option

    % exec shpfront -unknown -opts utf-8 t/fixture/utf-8.txt
    unhandled exception: Fail: unknown option: -unknown

# Stop option parsing

    % exec shpfront -opts utf-8 -- t/fixture/utf-8.txt | t/unpack.tcl
    %lst {%str 羊}

# Various types

    % exec echo {1 -1 3.14 -3.14 True true TRUE False false FALSE} | shpfront | t/unpack.tcl
    %lst 1 -1 3.14 -3.14 true true true false false false
