module type BST = sig
  type e
  type t

  val empty : t
  val height : t -> int
  val size : t -> int
  val add : t -> e -> t
  val find : t -> e -> e option
  val find_cmp : t -> compare:('a -> e -> int) -> 'a -> e option
  val range : t -> e -> e -> e list
  val range_cmp : t -> compare:('a -> e -> int) -> 'a -> 'a -> e list
  val remove : t -> e -> t
  val remove_cmp : t -> compare:('a -> e -> int) -> 'a -> t
  val update : t -> 'a -> compare:('a -> e -> int) -> f:(e -> e) -> t
  val pred : t -> e -> e option
  val succ : t -> e -> e option
  val is_empty : t -> bool
  val min_value : t -> e option
  val max_value : t -> e option
  val mem : t -> e -> bool
  val split_cmp : t -> compare:('a -> e -> int) -> 'a -> (t * e) option
  val split : t -> e -> (t * e) option
  val split_min : t -> (t * e) option
  val traverse : t -> e list
end
