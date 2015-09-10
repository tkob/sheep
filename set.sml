functor SetFun(eqtype elem) :> sig
  eqtype elem
  type set

  val empty : set
  val isEmpty : set -> bool
  val singleton : elem -> set
  val member : set -> elem -> bool
  val remove : set -> elem -> set
  val insert : set -> elem -> set
  val union : set -> set -> set
  val difference : set -> set -> set
  val toList : set -> elem list
end where type elem = elem = struct
  type elem = elem
  type set = elem list

  val empty = []
  val isEmpty = List.null
  fun singleton e = [e]
  fun member [] e = false
    | member (x::xs) e =
        x = e orelse member xs e
  fun remove [] e = []
    | remove (x::xs) e =
        if x = e then xs
        else x::remove xs e
  fun insert s e = if member s e then s else e::s
  fun union xs ys =
        List.foldl (fn (x, ys) => insert ys x) ys xs
  fun difference xs ys =
        List.foldl (fn (y, xs) => remove xs y) xs ys
  fun toList s = s
end
