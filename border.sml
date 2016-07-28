structure Border = struct
  datatype border = H of int * int | V of int * int

  fun manhattan (H (x, y)) = x + y
    | manhattan (V (x, y)) = x + y

  fun isNeighbour (H (x1, y1), H (x2, y2)) = y1 = y2 andalso abs (x1 - x2) = 1
    | isNeighbour (V (x1, y1), V (x2, y2)) = x1 = x2 andalso abs (y1 - y2) = 1
    | isNeighbour (V (vx, vy), H (hx, hy)) =
        (vx = hx andalso vy = hy)
        orelse (vx - 1 = hx andalso vy + 1 = hy)
        orelse (vx - 1 = hx andalso vy = hy)
        orelse (vx = hx andalso vy + 1 = hy)
    | isNeighbour (b1 as H (_, _), b2 as V (_, _)) = isNeighbour (b2, b1)

  fun groupByDistance (maxDistance, borders) =
        let
          val groups = Array.array (maxDistance + 1, [])
          val toList = Array.foldr (fn (a,b) => a::b) []
          fun loop [] = List.concat (toList groups)
            | loop (border::borders) =
                let
                  val distance = manhattan border
                  val group = Array.sub (groups, distance)
                  val group' = if List.exists (fn x => x = border) group
                               then group
                               else border::group
                in
                  Array.update (groups, distance, group')
                  handle Duplicate => ();
                  loop borders
                end
        in
          loop borders
        end

  (* val hasNeighbour : border * border list -> bool *)
  fun hasNeighbour (b, []) = false
    | hasNeighbour (b, b'::bs) =
        if manhattan b - manhattan b' > 1 then false
        else
          if isNeighbour (b, b') then true
          else hasNeighbour (b, bs)

  exception NoMerge

  (* val merge'' : border * border list list -> border list list *)
  (* merge'' (b, bss) = merges b into one of bss if it contains a neighbour of b.
     Otherwise, [b] is prepended to bss *)
  fun merge'' (b, []) = raise NoMerge
    | merge'' (b, bs::bss) =
        if hasNeighbour (b, bs) then (b::bs)::bss
        else bs::merge'' (b, bss)

  (* val merge' : border list * border list list -> border list list *)
  fun merge' ([], merge'd) = merge'd
    | merge' (b::bs, merge'd) =
        let
          val merge'd' = merge'' (b, merge'd) handle NoMerge => [b]::merge'd
        in
          merge' (bs, merge'd')
        end

  (* val merge : border list -> border list list *)
  (* merge bs = groups neighbours in bs *)
  fun merge bs = merge' (bs, [])
end
