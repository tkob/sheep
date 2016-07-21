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
