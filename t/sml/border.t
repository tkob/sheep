# insert

```
- use "border.sml";
...
val it = () : unit
- open Border;
...
```

```
- Border.insert (op >) (1, []);
val it = [1] : int list
- Border.insert (op >) (2, [1]);
val it = [2,1] : int list
- Border.insert (op >) (1, [2]);
val it = [2,1] : int list
- Border.insert (op >) (1, [3, 2]);
val it = [3,2,1] : int list
- Border.insert (op >) (2, [3, 1]);
val it = [3,2,1] : int list
```

# groupByDistance

```
- groupByDistance (3, [H (0, 0), V (0, 0), H (1, 0), V (1, 0), H (1, 1), V (1, 1)]);
val it = [H (0,0),V (0,0),H (1,0),V (1,0),H (1,1),V (1,1)] : border list
```

```
- groupByDistance (3, [H (0, 0), V (1, 1), H (0, 1), H (1, 0)]);
val it = [H (0,0),H (1,0),H (0,1),V (1,1)] : border list
```

# groupByDistance - duplicate

```
- groupByDistance (3, [H (0, 0), V (1, 1), V (1, 1), H (1, 0)]);
val it = [H (0,0),H (1,0),V (1,1)] : border list
```

# isNeighbour - true

```
- isNeighbour (H (0, 0), V (0, 0));
val it = true : bool
```

```
- isNeighbour (H (0, 1), V (1, 0));
val it = true : bool
```

```
- isNeighbour (H (0, 1), V (0, 0));
val it = true : bool
```

```
- isNeighbour (H (0, 0), V (1, 0));
val it = true : bool
```

# isNeighbour - false

```
- isNeighbour (H (0, 0), V (0, 1));
val it = false : bool
- isNeighbour (H (0, 0), V (~1, 0));
val it = false : bool
- isNeighbour (H (0, ~1), V (0, 0));
val it = false : bool
- isNeighbour (H (1, 0), V (0, 0));
val it = false : bool
```

```
- isNeighbour (H (0, 1), V (2, 0));
val it = false : bool
- isNeighbour (H (0, 1), V (1, ~1));
val it = false : bool
- isNeighbour (H (0, 2), V (1, 0));
val it = false : bool
- isNeighbour (H (~1, 1), V (1, 0));
val it = false : bool
```

```
- isNeighbour (H (0, 1), V (~1, 0));
val it = false : bool
- isNeighbour (H (0, 1), V (0, ~1));
val it = false : bool
- isNeighbour (H (0, 2), V (0, 0));
val it = false : bool
- isNeighbour (H (1, 1), V (0, 0));
val it = false : bool
```

```
- isNeighbour (H (0, 0), V (2, 0));
val it = false : bool
- isNeighbour (H (0, 0), V (1, 1));
val it = false : bool
- isNeighbour (H (0, ~1), V (1, 0));
val it = false : bool
- isNeighbour (H (~1, 0), V (1, 0));
val it = false : bool
```

# merge

```
- merge [];
val it = [] : border list list
```

```
- merge [H (0, 0)];
val it = [[H (0,0)]] : border list list
```

```
- merge [H (0, 0), V (1, 0)];
val it = [[V (1,0),H (0,0)]] : border list list
```

```
- merge (groupByDistance (1, [H (0, 0), V (1, 0)]));
val it = [[V (1,0),H (0,0)]] : border list list
```

```
- merge (groupByDistance (2, [H (0, 0), V (1, 0), V (2, 0)]));
val it = [[V (2,0)],[V (1,0),H (0,0)]] : border list list
```

```
- merge (groupByDistance (2, [H (0, 0), V (1, 0), V (2, 0)]));
val it = [[V (2,0)],[V (1,0),H (0,0)]] : border list list
```

```
- merge (groupByDistance (5, [H (0, 0), V (0, 0), V (1, 0), H (2, 0), V (2, 0), V (3, 0), H (0, 1), H (2, 1), V (2, 1), V (3, 1), H (2, 2)]));
val it =
  [[H (2,2),V (3,1),V (2,1),H (2,1),V (3,0),V (2,0),H (2,0)],
   [H (0,1),V (1,0),V (0,0),H (0,0)]] : border list list
```
