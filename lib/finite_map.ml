open Avl
open Adt
open Ord

module FiniteMap = struct
  module type Value = sig
    type t
  end

  module type S = sig
    type k
    type v
    type t

    val empty : t
    val is_empty : t -> bool
    val add : t -> k -> v -> t
    val update : t -> k -> f:(k -> v -> v) -> t
    val remove : t -> k -> t
    val size : t -> int
    val find : t -> k -> v option
    val range : t -> k -> k -> v list
    val min_value : t -> (k * v) option
    val max_value : t -> (k * v) option

    (* TODO: see if this is required
       val pred : t -> k -> (k * v) option
       val succ : t -> k -> (k * v) option *)
    val elem : t -> k -> bool
    val split : t -> k -> (t * (k * v)) option
    val split_min : t -> (t * (k * v)) option
    val to_list : t -> (k * v) list
  end

  module MakeWithTree (K : Ord) (V : Value) (T : Bst.BST with type e := K.t * V.t) :
    S with type k := K.t and type v := V.t = struct
    type t = T.t

    let cmp_key_and_kv k1 (k2, _) = K.compare k1 k2
    let drop_key (_, v) = v
    let empty = T.empty
    let is_empty = T.is_empty
    let add t k v = T.add t (k, v)

    let update tree k ~f =
      T.update tree k ~compare:cmp_key_and_kv ~f:(fun (k, v) -> k, f k v)
    ;;

    let remove = T.remove_cmp ~compare:cmp_key_and_kv
    let size = T.size
    let find tree k = T.find_cmp ~compare:cmp_key_and_kv tree k |> Option.map drop_key

    let range tree lower upper =
      T.range_cmp ~compare:cmp_key_and_kv tree lower upper |> List.map drop_key
    ;;

    let min_value = T.min_value
    let max_value = T.max_value
    let elem tree k = find tree k |> Option.is_some
    let split tree k = T.split_cmp ~compare:cmp_key_and_kv tree k
    let split_min = T.split_min
    let to_list = T.traverse
  end

  module Make (K : Ord) (V : Value) =
    MakeWithTree (K) (V)
      (AvlTree.Make (struct
           type t = K.t * V.t

           let compare (k1, _) (k2, _) = K.compare k1 k2
         end))
end
