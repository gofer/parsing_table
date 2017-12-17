(*
S  -> E $
E  -> T E'
E' -> + T E' | - T E' | ε
T  -> F T'
T' -> * F T' | / F T' | ε
F  -> ( E ) | num
*)

val S  = NonTerminalSymbol "S";
val E  = NonTerminalSymbol "E";
val Ed = NonTerminalSymbol "E'";
val T  = NonTerminalSymbol "T";
val Td = NonTerminalSymbol "T'";
val F  = NonTerminalSymbol "F";

val non_terminal_symbols = [S, E, Ed, T, Td, F];

val plus   = TerminalSymbol "+";
val minus  = TerminalSymbol "-";
val star   = TerminalSymbol "*";
val slash  = TerminalSymbol "/";
val lparen = TerminalSymbol "(";
val rparen = TerminalSymbol ")";
val num    = TerminalSymbol "num";
val dollar = TerminalSymbol "$";

val terminal_symbols = [plus, minus, star, slash, lparen, rparen, num, dollar];

val rule1  = (S,  [E, dollar]);
val rule2  = (E,  [T, Ed]);
val rule3  = (Ed, [plus,  T, Ed]);
val rule4  = (Ed, [minus, T, Ed]);
val rule5  = (Ed, []);
val rule6  = (T,  [F, Td]);
val rule7  = (Td, [star,  F, Td]);
val rule8  = (Td, [slash, F, Td]);
val rule9  = (Td, []);
val rule10 = (F,  [lparen, E, rparen]);
val rule11 = (F,  [num]);

val rule_list = [rule1, rule2, rule3, rule4, rule5, rule6, rule7, rule8, rule9, rule10, rule11] : Rule list;