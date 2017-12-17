datatype Symbol =    TerminalSymbol of string 
                | NonTerminalSymbol of string;

type Rule = Symbol * Symbol list;
