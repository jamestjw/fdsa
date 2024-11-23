module type Ord = sig
  type t

  val compare : t -> t -> int
end

module AvlTree = struct
  module type BST = sig
    type e
    type t

    val height : t -> int
    val size : t -> int
    val is_empty : t -> bool
    val min_value : t -> e option
    (* TODO: define what it means to be a BST *)
    (* TODO: e.g. define insert, delete and search *)
  end

  module Make (E : Ord) : BST with type e := E.t = struct
    type e = E.t
    type t = Empty | Node of int * t * e * t

    let height = function Empty -> 0 | Node (h, _, _, _) -> h

    let new_node left v right =
      let h = 1 + Int.max (height left) (height right) in
      Node (h, left, v, right)

    (* Difference in the heights of the left and right subtrees *)
    let bias = function
      | Empty -> 0
      | Node (_, left, _, right) -> height left - height right

    let rotate_left = function
      | Empty -> Empty
      | Node (_, l1, v1, Node (_, l2, v2, r2)) ->
          new_node (new_node l1 v1 l2) v2 r2
      | Node (_, _, _, Empty) -> failwith "rotate_left on invalid AVL tree"

    let rotate_right = function
      | Empty -> Empty
      | Node (_, Node (_, l1, v1, r1), v2, r2) ->
          new_node l1 v1 (new_node r1 v2 r2)
      | Node (_, Empty, _, _) -> failwith "rotate_right on invalid AVL tree"

    let avl_balance l v r =
      let hl = height l in
      let hr = height r in
      if hr + 1 < hl then
        (* Node is doubly left-heavy *)
        if bias l >= 0 then
          (* and left subtree is left-heavy or balanced *)
          new_node l v r |> rotate_right
        else new_node (rotate_left l) v r |> rotate_right
      else if hl + 1 < hr then
        (* Node is doubly right-heavy *)
        if bias r <= 0 then
          (* and right subtree is right-heavy or balanced *)
          new_node l v r |> rotate_left
        else new_node l v (rotate_right r) |> rotate_left
      else new_node l v r

    (* Splits the max element from the tree *)
    let rec split_max = function
      | Node (_, l, v, Empty) -> (l, v)
      | Node (_, l, v, r) ->
          let m, y = split_max r in
          (avl_balance l v m, y)
      | Empty -> failwith "cannot split empty tree"

    (* Merge two trees to produce a balanced tree *)
    let merge t1 t2 =
      match (t1, t2) with
      | l, Empty -> l
      | Empty, r -> r
      | l, r ->
          let l', x = split_max l in
          avl_balance l' x r

    let rec size = function
      | Empty -> 0
      | Node (_, l, _, r) -> 1 + size l + size r

    let rec find tree a =
      match tree with
      | Empty -> None
      | Node (_, l, v, _) when compare a v < 0 -> find l a
      | Node (_, _, v, r) when compare a v > 0 -> find r a
      | Node (_, _, v, _) -> Some v

    (* When an 'existing' element is added again to the tree, it takes the place
       of the one that is already there.*)
    let rec add tree a =
      match tree with
      | Empty -> new_node Empty a Empty
      | Node (_, l, v, r) when compare a v < 0 -> avl_balance (add l a) v r
      | Node (_, l, v, r) when compare a v > 0 -> avl_balance l v (add r a)
      | Node (h, l, _, r) -> Node (h, l, a, r)

    (* When the element is not in the tree, this function simple does nothing. *)
    let rec remove tree a =
      match tree with
      | Empty -> Empty
      | Node (_, l, v, r) when compare a v < 0 -> avl_balance (remove l a) v r
      | Node (_, l, v, r) when compare a v > 0 -> avl_balance l v (remove r a)
      | Node (_, l, _, r) -> merge l r

    let is_empty = function Empty -> true | _ -> false

    let rec min_value = function
      | Empty -> None
      | Node (_, Empty, v, _) -> Some v
      | Node (_, l, _, _) -> min_value l

    let rec max_value = function
      | Empty -> None
      | Node (_, _, v, Empty) -> Some v
      | Node (_, _, _, r) -> max_value r

    (* Finds the largest value that is smaller than e in the tree *)
    let pred tree e =
      let rec pred' tree e acc =
        match tree with
        | Empty -> acc
        | Node (_, l, v, _) when compare e v < 0 -> pred' l e acc
        | Node (_, _, v, r) when compare e v > 0 -> pred' r e (Some v)
        | Node (_, l, _, _) -> (
            match max_value l with None -> acc | Some ml -> Some ml)
      in
      pred' tree e None

    (* Finds the smallest value that is larger than e in the tree *)
    let succ tree e =
      let rec succ' tree e acc =
        match tree with
        | Empty -> acc
        | Node (_, l, v, _) when compare e v < 0 -> succ' l e (Some v)
        | Node (_, _, v, r) when compare e v > 0 -> succ' r e acc
        | Node (_, l, _, _) -> (
            match min_value l with None -> acc | Some ml -> Some ml)
      in
      succ' tree e None
  end
end
