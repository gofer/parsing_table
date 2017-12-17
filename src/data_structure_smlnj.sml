structure OrderdSymbol :> ORD_KEY 
  where type ord_key = Symbol
= struct
  type ord_key = Symbol;
  
  fun toString (NonTerminalSymbol s) = "(NonTerminalSymbol" ^ s ^ ")"
    | toString (   TerminalSymbol s) =    "(TerminalSymbol" ^ s ^ ")";
  
  fun compare (lhs, rhs) = 
    if (toString lhs) < (toString rhs)
      then LESS
      else 
        if (toString lhs) > (toString rhs)
          then GREATER
          else EQUAL; 
end;

structure SymbolSet = SetFunctor
  (
    OrderdSymbol
  );

structure SymbolBooleanHash = HashFunctor
  (
    OrderdSymbol
  )
  (
    type value_type = bool
  );

structure Nullable = SymbolBooleanHash;

structure SymbolSetHash = HashFunctor
  (
    OrderdSymbol
  )
  (
    type value_type = SymbolSet.set
  );

structure First  = SymbolSetHash;
structure Follow = SymbolSetHash;

structure OrderdRule :> ORD_KEY 
  where type ord_key = Rule
= struct
  type ord_key = Rule;
  
  fun symbolToString (NonTerminalSymbol s) = "(NonTerminalSymbol" ^ s ^ ")"
    | symbolToString (   TerminalSymbol s) =    "(TerminalSymbol" ^ s ^ ")"
  and toString (s, rs) = (symbolToString s) ^ " -> " ^ (String.concatWith " " (List.map symbolToString rs));
  
  fun compare (lhs, rhs) = 
    if (toString lhs) < (toString rhs)
      then LESS
      else 
        if (toString lhs) > (toString rhs)
          then GREATER
          else EQUAL; 
end;

structure RuleSet = SetFunctor
  (
    OrderdRule
  );

structure OrderdSymbolTuple :> ORD_KEY 
  where type ord_key = Symbol * Symbol
= struct
  type ord_key = Symbol * Symbol;
  
  fun symbolToString (NonTerminalSymbol s) = "(NonTerminalSymbol" ^ s ^ ")"
    | symbolToString (   TerminalSymbol s) =    "(TerminalSymbol" ^ s ^ ")"
  and toString (symbol1, symbol2) = "(" ^ (symbolToString symbol1) ^ ", " ^ (symbolToString symbol2) ^ ")"
  
  fun compare (lhs, rhs) = 
    if (toString lhs) < (toString rhs)
      then LESS
      else 
        if (toString lhs) > (toString rhs)
          then GREATER
          else EQUAL; 
end;

structure ParsingTable = HashFunctor
  (
    OrderdSymbolTuple
  )
  (
    type value_type = RuleSet.set
  );
