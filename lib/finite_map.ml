open Avl
open Ord

module FiniteMap = struct
  module type Value = sig
    type t
  end

  module Make (K : Ord) (V : Value) = struct
    module Tree = AvlTree.Make (struct
      type t = K.t * V.t

      let compare (k1, _) (k2, _) = K.compare k1 k2
    end)

    type t = Tree.t

    let cmp_key_and_kv k1 (k2, _) = K.compare k1 k2
    let drop_key (_, v) = v
    let is_empty = Tree.is_empty
    let add = Tree.add

    let update tree k ~f =
      Tree.update tree k ~compare:cmp_key_and_kv ~f:(fun (k, v) -> (k, f v))

    let remove = Tree.remove
    let size = Tree.size

    let find tree k =
      Tree.find_cmp ~compare:cmp_key_and_kv tree k |> Option.map drop_key

    let range tree lower upper =
      Tree.range_cmp ~compare:cmp_key_and_kv tree lower upper
      |> List.map drop_key

    let min_value = Tree.min_value
    let max_value = Tree.max_value
    let elem tree k = find tree k |> Option.is_some
    let split tree k = Tree.split_cmp ~compare:cmp_key_and_kv tree k
    let to_list = Tree.traverse
  end
end
