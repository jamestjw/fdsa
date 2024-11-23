module type Ord = sig
  type t

  val compare : t -> t -> int
end
