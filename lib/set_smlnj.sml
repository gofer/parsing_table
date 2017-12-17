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
    OrderdKey : ORD_KEY
  ) :> SET
  where type value_type = OrderdKey.ord_key
  =
struct
  type value_type = OrderdKey.ord_key;
  
  structure OrderdSet = SplaySetFn(OrderdKey);
  
  type set = OrderdSet.set;
  
  exception NoSuchElement;
  
  fun isEmpty set = OrderdSet.isEmpty set;
  
  val empty = OrderdSet.empty;

  fun member (set, item) = OrderdSet.member (set, item);
  
  fun add (set, item) = OrderdSet.add (set, item);
  
  fun delete (set, item) = 
    if member (set, item) 
      then OrderdSet.delete (set, item) 
      else raise NoSuchElement;
  
  fun union (set1, set2) = OrderdSet.union (set1, set2);
  
  fun intersection (set1, set2) = OrderdSet.intersection (set1, set2);
  
  fun equal (set1, set2) = OrderdSet.equal (set1, set2);
  
  fun listItems set = OrderdSet.listItems set;
end;

(*
structure OrderdString :> ORD_KEY 
  where type ord_key = String.string
= struct
  type ord_key = String.string;
  
  fun compare (lhs, rhs) = 
    if String.< (lhs, rhs)
      then LESS
      else 
        if String.> (lhs, rhs)
          then GREATER
          else EQUAL; 
end;

structure StringSet = SetFunctor
  (
    OrderdString
  );
*)