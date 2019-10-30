signature MAP = sig
  exception NoSuchKey;
  
  eqtype   key_type;
    type value_type;
    
  type map;
  
  val empty      : map;
  val insert     : map * key_type * value_type -> map;
  val find       : map * key_type -> value_type;
  val keys       : map -> key_type list;
  val equal      : ((value_type * value_type) -> bool) -> (map * map) -> bool;
  val filter     : (value_type -> bool) -> map -> map;
  val filteri    : ((key_type * value_type) -> bool) -> map -> map;
  val listItems  : map -> value_type list;
  val listItemsi : map -> (key_type * value_type) list;
end;

functor MapFunctor 
  (
    OrdKey : ORD_KEY
  )
  (
    type value_type
  ) :> MAP 
  where type   key_type = OrdKey.ord_key
  where type value_type = value_type
  = 
struct
  exception NoSuchKey;
  
  type   key_type = OrdKey.ord_key;
  type value_type = value_type;
  
  structure HashMap = SplayMapFn (OrdKey);
  
  type map = value_type HashMap.map;
  
  val empty = HashMap.empty;
  
  fun insert (map, key, value) = HashMap.insert (map, key, value);
  
  fun find (map, key) = Option.valOf (HashMap.find (map, key))
                         handle Option => raise NoSuchKey;
  
  fun keys map = List.map (fn (key, value) => key) (HashMap.listItemsi map);
  
  fun equal pred (map1, map2) = let
    fun sameValueByEachKey pred (map1, map2) =
      List.all 
        (fn x => x) 
        (
          List.map 
            (fn k => pred (find (map1, k), find (map2, k))) 
            (keys map1)
        ) 
      handle NoSuchKey => false;
  in
    if (HashMap.numItems map1) = (HashMap.numItems map2)
      then sameValueByEachKey pred (map1, map2)
      else false
  end;
  
  fun filter  pred map = HashMap.filter  pred map;
  fun filteri pred map = HashMap.filteri pred map;
  
  fun listItems  map = HashMap.listItems  map;
  fun listItemsi map = HashMap.listItemsi map;
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

structure IntegerMap = MapFunctor 
  (OrdInteger) 
  (type value_type = string);
*)

