(*
E -> I E'
E' -> - I E' | ε
I -> x | y | z
*)

val E  = NonTerminalSymbol "E";
val Ed = NonTerminalSymbol "E'";
val I  = NonTerminalSymbol "I";

val non_terminal_symbols = [E, Ed, I];

val minus  = TerminalSymbol "-";
val x      = TerminalSymbol "x";
val y      = TerminalSymbol "y";
val z      = TerminalSymbol "z";
val dollar = TerminalSymbol "$";

val terminal_symbols = [minus, x, y, z, dollar];

val rule1 = (E, [I, Ed, dollar]);
val rule2 = (Ed, [minus, I, Ed]);
val rule3 = (Ed, nil);
val rule4 = (I, [x]);
val rule5 = (I, [y]);
val rule6 = (I, [z]);

val rule_list = [rule1, rule2, rule3, rule4, rule5, rule6] : Rule list;