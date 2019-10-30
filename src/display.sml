fun listToString to_string xs = "[" ^ (String.concatWith ", " (List.map to_string xs)) ^ "]";

fun symbolToString (   TerminalSymbol s) = s
  | symbolToString (NonTerminalSymbol s) = s;

fun ruleToString (s, rs) = (symbolToString s) ^ " -> " ^ (String.concatWith " " (List.map symbolToString rs));

fun hashToString to_string hash = let
  fun tupleToString (k, v) = (symbolToString k) ^ ": " ^ (to_string v);
in listToString tupleToString hash end;

fun symbolBooleanMapToString hash = 
  hashToString 
    Bool.toString 
    (SymbolBooleanMap.listItemsi hash);

fun symbolSetMapToString hash = 
  hashToString 
    (fn v => listToString symbolToString (SymbolSet.listItems v)) 
    (SymbolSetMap.listItemsi hash);

fun parsingTableToString parsing_table =
  listToString
    (
      fn ((s, t), rules) => 
        "(" ^ (symbolToString s) ^ ", " ^ (symbolToString t) ^ "): " 
        ^ (listToString ruleToString (RuleSet.listItems rules))
    )
    (ParsingTable.listItemsi parsing_table);

fun parsingTableToHtmlString parsing_table non_terminal_symbols terminal_symbols = let
  fun stringListConcat ss = List.foldr String.^ "" ss;
  
  fun withTd s = "<td><code><pre>" ^ s ^ "</pre></code></td>"
  and withTr s = "<tr>" ^ s ^ "</tr>\n"
  and lineToString h ss = withTr ((withTd h) ^ (stringListConcat (List.map (fn s => withTd s) ss)));
  
  val header = lineToString  "\\" (List.map symbolToString terminal_symbols);
  
  fun g symbol1 symbol2 = let
    fun searchTableBySymbol parsing_table symbol1 symbol2 = 
      ParsingTable.listItemsi
        (
          ParsingTable.filteri 
            (fn ((x1, x2), ys) => x1 = symbol1 andalso x2 = symbol2) 
            parsing_table
        );
  
    fun ruleListToString rs = String.concatWith "<br>" (List.map ruleToString rs);
    val ls = searchTableBySymbol parsing_table symbol1 symbol2;
    val ls = List.map (fn ((x1, x2), ys) => RuleSet.listItems ys) ls;
    val ls = List.map ruleListToString ls;
  in stringListConcat ls end
  and f symbol1 = let
    val ls = List.map (fn symbol2 => g symbol1 symbol2) terminal_symbols;
  in lineToString (symbolToString symbol1) ls end;
  
  val body = List.map (fn symbol1 => f symbol1) non_terminal_symbols;
  val body = stringListConcat body;
in 
  "<table border=\"1\">\n" ^ 
  "<thead>\n" ^ header ^ "</thead>\n" ^
  "<tbody>\n" ^ body   ^ "</tbody>\n" ^
  "</table>"
end;
