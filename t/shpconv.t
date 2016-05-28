# from UTF-8

    % exec env LANG=en_US.UTF-8 shpconv.tcl t/fixture/utf-8.txt
    羊

# from Shift_JIS

    % exec shpconv.tcl -encoding shiftjis t/fixture/shiftjis.txt
    羊

# from stdin

    % exec shpconv.tcl -encoding utf-8 < t/fixture/utf-8.txt
    羊

# multiple files

    % exec shpconv.tcl -encoding utf-8 t/fixture/utf-8.txt t/fixture/utf-8.txt
    羊
    羊
