# Employees

## Total pay for each employee who worked more than 0 hour

    % exec shp -e {name, wage, hours when hours > 0 { ! name, wage * hours }} t/fixture/emp.data
    Kathy 40.0 
    Mark 100.0 
    Mary 121.0 
    Susie 76.5 

## Employees who did not work (use guard)

    % exec shp -e {name, _, hours when hours = 0 { ! name }} t/fixture/emp.data
    Beth 
    Dan 

## Employees who did not work (use pattern matching)

    % exec shp -e {name, _, 0 { ! name }} t/fixture/emp.data
    Beth 
    Dan 

## Print NR for each record

    % exec shp -e {... { ! NR }} t/fixture/emp.data
    1 
    2 
    3 
    4 
    5 
    6 

## Print number of records

    % exec shp -e {END { ! NR }} t/fixture/emp.data
    6 
