# insert

```
> use "border.sml";
...
val it = (): unit
```

```
> Border.insert (op >) (1, []);
val it = [1]: int list
> Border.insert (op >) (2, [1]);
val it = [2, 1]: int list
> Border.insert (op >) (1, [2]);
val it = [2, 1]: int list
> Border.insert (op >) (1, [3, 2]);
val it = [3, 2, 1]: int list
> Border.insert (op >) (2, [3, 1]);
val it = [3, 2, 1]: int list
```

# groupByDistance

```
> open Border;
...
> groupByDistance (3, [H (0, 0), V (0, 0), H (1, 0), V (1, 0), H (1, 1), V (1, 1)]);
val it = [[H (0, 0), V (0, 0)], [H (1, 0), V (1, 0)], [H (1, 1), V (1, 1)]]:
   border list list
```

```
> groupByDistance (3, [H (0, 0), V (1, 1), H (0, 1), H (1, 0)]);
val it = [[H (0, 0)], [H (1, 0), H (0, 1)], [V (1, 1)]]: border list list
```

# groupByDistance - duplicate

```
> groupByDistance (3, [H (0, 0), V (1, 1), V (1, 1), H (1, 0)]);
val it = [[H (0, 0)], [H (1, 0)], [V (1, 1)]]: border list list
```
