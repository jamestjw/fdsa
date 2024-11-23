module type BST = sig
  type e
  type t

  val height : t -> int
  val size : t -> int
  val add : t -> e -> t
  val find : t -> e -> e option
  val remove : t -> e -> t
  val pred : t -> e -> e option
  val succ : t -> e -> e option
  val is_empty : t -> bool
  val min_value : t -> e option
end
