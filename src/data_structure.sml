structure SymbolSet = SetFunctor
  (
    type value_type = Symbol
  );

structure SymbolBooleanHash = HashFunctor
  (
    type   key_type = Symbol
    type value_type = bool
  );

structure Nullable = SymbolBooleanHash;

structure SymbolSetHash = HashFunctor
  (
    type   key_type = Symbol
    type value_type = SymbolSet.set
  );

structure First  = SymbolSetHash;
structure Follow = SymbolSetHash;

structure RuleSet = SetFunctor
  (
    type value_type = Rule
  );

structure ParsingTable = HashFunctor
  (
    type   key_type = Symbol * Symbol
    type value_type = RuleSet.set
  );