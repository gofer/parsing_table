(*
E -> E - I $ | I $
I -> x | y | z
*)

val E = NonTerminalSymbol "E";
val I = NonTerminalSymbol "I";

val non_terminal_symbols = [E, I];

val minus  = TerminalSymbol "-";
val x      = TerminalSymbol "x";
val y      = TerminalSymbol "y";
val z      = TerminalSymbol "z";
val dollar = TerminalSymbol "$";

val terminal_symbols = [minus, x, y, z, dollar];

val rule1 = (E, [E, minus, I, dollar]);
val rule2 = (E, [I, dollar]);
val rule3 = (I, [x]);
val rule4 = (I, [y]);
val rule5 = (I, [z]);

val rule_list = [rule1, rule2, rule3, rule4, rule5] : Rule list;