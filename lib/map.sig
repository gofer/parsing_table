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