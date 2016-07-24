structure Border = struct
  datatype border = H of int * int | V of int * int

  fun manhattan (H (x, y)) = x + y
    | manhattan (V (x, y)) = x + y

  fun gt (H (x1, _), H (x2, _)) = x1 > x2
    | gt (H (x1, _), V (x2, _)) = if x1 = x2 then true else x1 > x2
    | gt (V (x1, _), V (x2, _)) = x1 > x2
    | gt (V (x1, _), H (x2, _)) = if x1 = x2 then false else x1 > x2

  exception Duplicate

  fun insert gt (x, []) = x::[]
    | insert gt (x, y::ys) =
        if gt (x, y) then x::y::ys
        else if x = y then raise Duplicate
        else y::(insert gt (x, ys))

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
          val groups = Array.array (maxDistance, [])
          val toList = Array.foldr (fn (a,b) => a::b) []
          fun loop [] = toList groups
            | loop (border::borders) =
                let
                  val distance = manhattan border
                  val group = Array.sub (groups, distance)
                in
                  Array.update (groups, distance, insert gt (border, group))
                  handle Duplicate => ();
                  loop borders
                end
        in
          loop borders
        end
end
