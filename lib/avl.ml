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

    let rec size = function
      | Empty -> 0
      | Node (_, l, _, r) -> 1 + size l + size r

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

    (* let rec find tree e = match tree with
       | Empty -> None *)

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
  end
end
