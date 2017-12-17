fun initialize_nullable symbols = 
  List.foldl 
    (fn (symbol, nullable) => Nullable.insert (nullable, symbol, false)) 
    Nullable.empty symbols;

fun do_refresh_nullable nullable ((x, nil) : Rule) = Nullable.insert (nullable, x, true)
  | do_refresh_nullable nullable ((x,  ys) : Rule) = let
    val f = List.all (fn x => x) (List.map (fn y => Nullable.find (nullable, y)) ys);
    val g = Nullable.find (nullable, x);
  in Nullable.insert (nullable, x, (f orelse g)) end
and refresh_nullable nullable rule_list = let
  val new_nullable = List.foldl (fn (rule, hash) => do_refresh_nullable hash rule) nullable rule_list;
in 
  if Nullable.equal (op =) (nullable, new_nullable)
    then new_nullable
    else refresh_nullable new_nullable rule_list
end;
