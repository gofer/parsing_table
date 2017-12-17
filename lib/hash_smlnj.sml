signature HASH = sig
  exception NoSuchKey;
  
  eqtype   key_type;
    type value_type;
    
  type hash;
  
  val empty      : hash;
  val insert     : hash * key_type * value_type -> hash;
  val find       : hash * key_type -> value_type;
  val keys       : hash -> key_type list;
  val equal      : ((value_type * value_type) -> bool) -> (hash * hash) -> bool;
  val filter     : (value_type -> bool) -> hash -> hash;
  val filteri    : ((key_type * value_type) -> bool) -> hash -> hash;
  val listItems  : hash -> value_type list;
  val listItemsi : hash -> (key_type * value_type) list;
end;

functor HashFunctor 
  (
    OrdKey : ORD_KEY
  )
  (
    type value_type
  ) :> HASH 
  where type   key_type = OrdKey.ord_key
  where type value_type = value_type
  = 
struct
  exception NoSuchKey;
  
  type   key_type = OrdKey.ord_key;
  type value_type = value_type;
  
  structure HashMap = SplayMapFn (OrdKey);
  
  type hash = value_type HashMap.map;
  
  val empty = HashMap.empty;
  
  fun insert (hash, key, value) = HashMap.insert (hash, key, value);
  
  fun find (hash, key) = Option.valOf (HashMap.find (hash, key))
                         handle Option => raise NoSuchKey;
  
  fun keys hash = List.map (fn (key, value) => key) (HashMap.listItemsi hash);
  
  fun equal pred (hash1, hash2) = let
    fun sameValueByEachKey pred (hash1, hash2) =
      List.all 
        (fn x => x) 
        (
          List.map 
            (fn k => pred (find (hash1, k), find (hash2, k))) 
            (keys hash1)
        ) 
      handle NoSuchKey => false;
  in
    if (HashMap.numItems hash1) = (HashMap.numItems hash2)
      then sameValueByEachKey pred (hash1, hash2)
      else false
  end;
  
  fun filter  pred hash = HashMap.filter  pred hash;
  fun filteri pred hash = HashMap.filteri pred hash;
  
  fun listItems  hash = HashMap.listItems  hash;
  fun listItemsi hash = HashMap.listItemsi hash;
end;

(*
structure OrdInteger : ORD_KEY = 
struct
  type ord_key = Int.int;
  
  fun compare (lhs, rhs) = 
    if lhs = rhs
      then EQUAL
      else
        if lhs < rhs
          then LESS
          else GREATER;
end;

structure IntegerHash = HashFunctor 
  (OrdInteger) 
  (type value_type = string);
*)

