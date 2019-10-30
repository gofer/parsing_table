structure SymbolSet = SetFunctor
  (
    type value_type = Symbol
  );

structure SymbolBooleanMap = MapFunctor
  (
    type   key_type = Symbol
    type value_type = bool
  );

structure Nullable = SymbolBooleanMap;

structure SymbolSetMap = MapFunctor
  (
    type   key_type = Symbol
    type value_type = SymbolSet.set
  );

structure First  = SymbolSetMap;
structure Follow = SymbolSetMap;

structure RuleSet = SetFunctor
  (
    type value_type = Rule
  );

structure ParsingTable = MapFunctor
  (
    type   key_type = Symbol * Symbol
    type value_type = RuleSet.set
  );
