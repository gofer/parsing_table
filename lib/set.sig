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
