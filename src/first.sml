fun initialize_first symbols = 
  List.foldl
    (
      fn (symbol, first) => 
        First.insert 
        (
          first, 
          symbol,
          (
            case (symbol)
              of (   TerminalSymbol s) => SymbolSet.add (SymbolSet.empty, symbol)
               | (NonTerminalSymbol s) => SymbolSet.empty
          )
        )
    )
    First.empty
    symbols;

fun do_refresh_first first nullable ((x, ys) : Rule) = let
  val first_x_set = First.find (first, x);
  
  fun subList pred nil = nil
    | subList pred (y::ys) = if (pred y) then y::(subList pred ys) else [y];

  val zs = subList (fn y => Nullable.find (nullable, y)) ys;
   
  val first_x_set = List.foldl 
    (
      fn (symbol, set) => 
        SymbolSet.union(set, First.find(first, symbol))
    )
    first_x_set zs;
in First.insert (first, x, first_x_set) end
and refresh_first first nullable rule_list = let
  val new_first = List.foldl 
    (
      fn (rule, hash) => 
        do_refresh_first hash nullable rule
    )
    first rule_list;
in
  if First.equal SymbolSet.equal (first, new_first)
    then new_first
    else refresh_first new_first nullable rule_list
end;