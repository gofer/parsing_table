fun initialize_follow symbols = 
  List.foldl
    (fn (symbol, hash) => Follow.insert (hash, symbol, SymbolSet.empty))
    Follow.empty
    symbols;

fun do_refresh_follow follow first nullable ((x, ys) : Rule) = let
  fun isNullableList ys = let
      val zs = List.map (fn y => Nullable.find (nullable, y)) ys;
  in List.all (fn x => x) zs end
  and refresh_follow_1 follow ((x, ys) : Rule) = let
    fun subList v = let
      val f = fn v => List.tabulate (List.length v, (fn x => x));
      val u = List.map (fn n => List.drop (v, n)) (f v);
      val u = List.map (fn t => (hd t, tl t)) u;
    in u end;
    
    val zs = List.filter (fn (y, ys) => isNullableList ys) (subList ys);
    
    val follow = List.foldl
      (
        fn ((y, ys), follow) => 
          Follow.insert
            (
              follow,
              y,
              (
                SymbolSet.union
                (
                  Follow.find (follow, y), 
                  Follow.find (follow, x)
                )
              )
            )
      )
      follow zs;
  in follow end
  and refresh_follow_2 follow ((x, ys) : Rule) = let
    fun subList v = let
      val f0 = fn v => List.tabulate (List.length v, (fn x => x + 0));
      val f1 = fn v => List.tabulate (List.length v, (fn x => x + 1));
      
      fun g (x::xs) = let
        val ys = List.rev xs;
        val y = List.hd ys;
        val ys = List.rev (List.tl ys);
      in (x, ys, y) end;
      
      val u = List.map (fn n => List.take (v, n)) (f1 v);
      val u = List.map (fn v => List.map (fn n => List.drop (v, n)) (f0 v)) u;
      val u = List.foldl List.@ nil u;
      val u = List.filter (fn v => List.length v > 1) u;
      val u = List.map (fn v => g v) u;
    in u end;
    
    val zs = List.filter (fn (x1, ys, x2) => isNullableList ys) (subList ys);
    
    val follow = List.foldl 
      (
        fn ((x1, ys, x2), follow) => 
          Follow.insert
            (
              follow,
              x1,
              (
                SymbolSet.union
                (
                  Follow.find (follow, x1),
                  Follow.find (first,  x2)
                )
              )
            )
      )
      follow
      zs;
  in follow end;
  
  val follow = refresh_follow_1 follow ((x, ys) : Rule);
  val follow = refresh_follow_2 follow ((x, ys) : Rule);
in follow end
and refresh_follow follow first nullable rule_list = let
  val new_follow = List.foldl 
    (
      fn (rule, hash) => 
        do_refresh_follow hash first nullable rule
    )
    follow rule_list;
in
  if Follow.equal SymbolSet.equal (follow, new_follow)
    then new_follow
    else refresh_follow new_follow first nullable rule_list
end;