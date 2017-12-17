val symbols = non_terminal_symbols @ terminal_symbols;

val nullable = initialize_nullable symbols;
val nullable = refresh_nullable nullable rule_list;

val first = initialize_first symbols;
val first = refresh_first first nullable rule_list;

val follow = initialize_follow symbols;
val follow = refresh_follow follow first nullable rule_list;

val parsing_table = 
  build_parsing_table 
    non_terminal_symbols terminal_symbols rule_list 
    nullable first follow;

fun isNonTerminalSymbol x = 
  case (x) 
  of (NonTerminalSymbol s) => true
   | (   TerminalSymbol s) => false;

val nullable = SymbolBooleanHash.filteri (fn (x, ys) => isNonTerminalSymbol x) nullable;
val    first =     SymbolSetHash.filteri (fn (x, ys) => isNonTerminalSymbol x)    first;
val   follow =     SymbolSetHash.filteri (fn (x, ys) => isNonTerminalSymbol x)   follow;

val () = print ("nullable = " ^ (symbolBooleanHashToString nullable) ^ "\n");
val () = print ("FIRST    = " ^ (    symbolSetHashToString    first) ^ "\n");
val () = print ("FOLLOW   = " ^ (    symbolSetHashToString   follow) ^ "\n");

val () = print ("ParsingTable = " ^ (parsingTableToString parsing_table) ^ "\n");

val html = parsingTableToHtmlString parsing_table non_terminal_symbols terminal_symbols;
val () = print (html ^ "\n");
