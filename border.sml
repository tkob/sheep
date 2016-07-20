structure Border = struct
  datatype border = H of int * int | V of int * int

  fun manhattan (H (x, y)) = x + y
    | manhattan (V (x, y)) = x + y

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
                  Array.update (groups, distance, border::group);
                  loop borders
                end
        in
          loop borders
        end
end
