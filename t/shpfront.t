# from UTF-8

    % exec env LANG=en_US.UTF-8 shpfront t/fixture/utf-8.txt
    {%str "羊"}

# from Shift_JIS

    % exec shpfront -opts encoding=shiftjis t/fixture/shiftjis.txt
    {%str "羊"}

# from stdin

    % exec shpfront -opts encoding=utf-8 < t/fixture/utf-8.txt
    {%str "羊"}

# multiple files

    % exec shpfront -opts encoding=utf-8 t/fixture/utf-8.txt t/fixture/utf-8.txt
    {%str "羊"}
    {%str "羊"}

# Specify format explicitly

    % exec shpfront -opts awk:encoding=utf-8 t/fixture/utf-8.txt
    {%str "羊"}

# Unknown format

    % exec shpfront -opts unknown:encoding=utf-8 t/fixture/utf-8.txt
    unhandled exception: Fail: unknown format: unknown

# Unknown option

    % exec shpfront -unknown -opts utf-8 t/fixture/utf-8.txt
    unhandled exception: Fail: unknown option: -unknown

# Stop option parsing

    % exec shpfront -opts utf-8 -- t/fixture/utf-8.txt
    {%str "羊"}
