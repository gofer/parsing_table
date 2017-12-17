fun product     nil ys = nil
  | product (x::xs) ys = (List.map (fn y => (x, y)) ys) @ product xs ys;

fun initialize_parsing_table non_terminal_symbols terminal_symbols = 
  List.foldl
    (fn (key, parsing_table) => ParsingTable.insert (parsing_table, key, RuleSet.empty))
    ParsingTable.empty
    (product non_terminal_symbols terminal_symbols);

fun do_build_parsing_table nullable first follow parsing_table rule_list symbol = let
  val rules = List.filter (fn (x, ys) => x = symbol) rule_list;
  
  fun parsing_table_insert parsing_table key value = let
    val old_set = ParsingTable.find (parsing_table, key);
    val new_set = RuleSet.add (old_set, value);
  in ParsingTable.insert (parsing_table, key, new_set) end;
  
  fun process1 parsing_table rules = let
    fun get_first_symbols ys = let
      val zs = List.map (fn y => (First.find (first, y), Nullable.find (nullable, y))) ys;
      
      fun do_get_first_symbols               nil = SymbolSet.empty
        | do_get_first_symbols ((set, flag)::vs) = 
          if flag 
            then SymbolSet.union (set, (do_get_first_symbols vs)) 
            else set;
    in do_get_first_symbols zs end;
    
    val zs = List.map (fn (x, ys) => ((x, ys), get_first_symbols ys)) rules;
    
    fun q (x, ys) symbols parsing_table = 
      List.foldl
        (fn (symbol, parsing_table) => parsing_table_insert parsing_table (x, symbol) (x, ys))
        parsing_table
        (SymbolSet.listItems symbols);
    
    val parsing_table = 
      List.foldl 
        (fn ((rule, symbols), parsing_table) => q rule symbols parsing_table) 
        parsing_table
        zs;
  in parsing_table end;
  
  fun process2 parsing_table rules = let
    fun isNullableList ys = Nullable.find (nullable, (hd ys)) handle Empty => true;
    
    val nullable_rules = List.filter (fn (x, ys) => isNullableList ys) rules;
    
    fun q (x, ys) parsing_table = let
      val follow_x = Follow.find (follow, x);
      val parsing_table = 
        List.foldl 
          (fn (symbol, parsing_table) => parsing_table_insert parsing_table (x, symbol) (x, ys))
          parsing_table
          (SymbolSet.listItems follow_x);
    in parsing_table end;
    
    val parsing_table = 
      List.foldl 
        (fn ((x, ys), parsing_table) => q (x, ys) parsing_table) 
        parsing_table
        nullable_rules;
  in parsing_table end;
  
  val parsing_table = process1 parsing_table rules;
  val parsing_table = process2 parsing_table rules;
in parsing_table end;

fun build_parsing_table non_terminal_symbols terminal_symbols rule_list nullable first follow = 
  List.foldl 
    (
      fn (symbol, parsing_table) => 
        do_build_parsing_table 
          nullable first follow 
          parsing_table rule_list symbol
    ) 
    (initialize_parsing_table non_terminal_symbols terminal_symbols) 
    non_terminal_symbols;
