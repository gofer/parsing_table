signature SET = sig
  eqtype value_type;
  
  eqtype set;
  
  exception NoSuchElement;
  
  val isEmpty      : set -> bool;
  val empty        : set;
  val member       : set * value_type -> bool;
  val add          : set * value_type -> set;
  val delete       : set * value_type -> set;
  val union        : set * set -> set;
  val intersection : set * set -> set;
  val equal        : set * set -> bool;
  val listItems    : set -> value_type list;
end;

functor SetFunctor
  (
    eqtype value_type
  ) :> SET
  where type value_type = value_type
  where type        set = value_type list
  =
struct
  type value_type = value_type;
  
  type set = value_type list;
  
  exception NoSuchElement;
  
  fun isEmpty nil = true
    | isEmpty   _ = false;
  
  val empty = nil;
  
  fun member (    nil,    _) = false
    | member ((s::ss), item) = if s = item then true else (member (ss, item));
  
  fun add (s, item) = if member (s, item) then s else item::s;
  
  fun delete (nil    , item) = raise NoSuchElement
    | delete ((s::ss), item) = if s = item then ss else s::(delete (ss, item));
  
  fun union (set,     nil) = set
    | union (set, (t::ts)) = if member (set, t) then union (set, ts) else union ((t::set), ts);
  
  fun intersection (set, nil) = nil
    | intersection (set, (t::ts)) = let
      val u = intersection(set, ts);
    in if member (set, t) then t::u else u end;
  
  fun equal (set1, set2) = let
    fun equal_aux (set, nil) = true
      | equal_aux (set, (t::ts)) = if member (set, t) then equal_aux(set, ts) else false;
  in if (List.length set1) = (List.length set2) then equal_aux (set1, set2) else false end;
  
  fun listItems set = set;
end;

(*
structure IntSet = SetFunctor
  (
    type value_type = int
  );
*)
