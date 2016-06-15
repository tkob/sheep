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

# newline is CR

    % exec shpconv.tcl -translation cr t/fixture/cr.txt
    sheep
    sheep

# newline is CRLF

    % exec shpconv.tcl -translation crlf t/fixture/crlf.txt
    sheep
    sheep

# newline is LF

    % exec shpconv.tcl -translation lf t/fixture/lf.txt
    sheep
    sheep
