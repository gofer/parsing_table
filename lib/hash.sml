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
    eqtype   key_type
      type value_type
  ) :> HASH 
  where type   key_type =   key_type
  where type value_type = value_type
  where type       hash = (key_type * value_type) list
  = 
struct
  exception NoSuchKey;

  type   key_type =   key_type;
  type value_type = value_type;
  
  type hash = (key_type * value_type) list;

  val empty = nil : hash;

  fun insert (nil, key, value) = [(key, value)]
    | insert (((k, v)::hs), key, value) = 
      if k = key 
        then (key, value)::hs 
        else (k, v)::(insert (hs, key, value));

  fun find (nil, key) = raise NoSuchKey
    | find (((k, v)::hs), key) = if k = key then v else find (hs, key);
  
  fun keys nil = nil
    | keys ((k, v)::es) = k::(keys es);
  
  fun equal cmp (a, b) = let
    fun equal_aux cmp (           nil, b) = true
      | equal_aux cmp (((ak, av)::aa), b) = 
        if (cmp((find (b, ak)), av) handle NoSuchKey => false) 
          then equal_aux cmp (aa, b) 
          else false;
  in 
    if (List.length a = List.length b) 
      then equal_aux cmp (a, b) 
      else false
  end;
  
  fun filter  pred nil = nil
    | filter  pred ((k, v)::hs) = if pred v then (k, v)::(filter pred hs) else filter pred hs;
  
  fun filteri pred nil = nil
    | filteri pred (h::hs) = if pred h then h::(filteri pred hs) else filteri pred hs;
  
  fun listItems nil = nil
    | listItems ((k,v)::hs) = v::(listItems hs);
  
  fun listItemsi hash = hash;
end;

(*
structure IntegerHash = HashFunctor
  (
    type   key_type = string
    type value_type = int
  );
*)
