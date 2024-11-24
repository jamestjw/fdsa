open Adt.Bst
open Ord

module AvlTree = struct
  module Make (E : Ord) : BST with type e := E.t = struct
    type e = E.t

    type t =
      | Empty
      | Node of int * t * e * t

    let compare = E.compare

    let height = function
      | Empty -> 0
      | Node (h, _, _, _) -> h
    ;;

    let new_node left v right =
      let h = 1 + Int.max (height left) (height right) in
      Node (h, left, v, right)
    ;;

    (* Difference in the heights of the left and right subtrees *)
    let bias = function
      | Empty -> 0
      | Node (_, left, _, right) -> height left - height right
    ;;

    let rotate_left = function
      | Empty -> Empty
      | Node (_, l1, v1, Node (_, l2, v2, r2)) -> new_node (new_node l1 v1 l2) v2 r2
      | Node (_, _, _, Empty) -> failwith "rotate_left on invalid AVL tree"
    ;;

    let rotate_right = function
      | Empty -> Empty
      | Node (_, Node (_, l1, v1, r1), v2, r2) -> new_node l1 v1 (new_node r1 v2 r2)
      | Node (_, Empty, _, _) -> failwith "rotate_right on invalid AVL tree"
    ;;

    let avl_balance l v r =
      let hl = height l in
      let hr = height r in
      if hr + 1 < hl
      then
        (* Node is doubly left-heavy *)
        if bias l >= 0
        then
          (* and left subtree is left-heavy or balanced *)
          new_node l v r |> rotate_right
        else new_node (rotate_left l) v r |> rotate_right
      else if hl + 1 < hr
      then
        (* Node is doubly right-heavy *)
        if bias r <= 0
        then
          (* and right subtree is right-heavy or balanced *)
          new_node l v r |> rotate_left
        else new_node l v (rotate_right r) |> rotate_left
      else new_node l v r
    ;;

    (* Splits the max element from the tree *)
    let rec split_max = function
      | Node (_, l, v, Empty) -> l, v
      | Node (_, l, v, r) ->
        let m, y = split_max r in
        avl_balance l v m, y
      | Empty -> failwith "cannot split empty tree"
    ;;

    (* Merge two trees to produce a balanced tree *)
    let merge t1 t2 =
      match t1, t2 with
      | l, Empty -> l
      | Empty, r -> r
      | l, r ->
        let l', x = split_max l in
        avl_balance l' x r
    ;;

    let rec size = function
      | Empty -> 0
      | Node (_, l, _, r) -> 1 + size l + size r
    ;;

    let rec find_cmp tree ~compare k =
      match tree with
      | Empty -> None
      | Node (_, l, v, _) when compare k v < 0 -> find_cmp l ~compare k
      | Node (_, _, v, r) when compare k v > 0 -> find_cmp r ~compare k
      | Node (_, _, v, _) -> Some v
    ;;

    let find = find_cmp ~compare:E.compare
    let mem tree e = find tree e |> Option.is_some

    (* Returns a list of values that are within a certain range (inclusive).
       Comparison is done using a custom compare function. *)
    let range_cmp tree ~compare lower upper =
      let rec range_cmp' tree lower upper acc =
        match tree with
        | Empty -> acc
        | Node (_, _, v, r) when compare lower v < 0 -> range_cmp' r lower upper acc
        | Node (_, l, v, _) when compare upper v > 0 -> range_cmp' l lower upper acc
        | Node (_, l, v, r) -> range_cmp' l lower upper (v :: range_cmp' r lower upper acc)
      in
      range_cmp' tree lower upper []
    ;;

    let range = range_cmp ~compare:E.compare

    (* When an 'existing' element is added again to the tree, it takes the place
       of the one that is already there.*)
    let rec add tree a =
      match tree with
      | Empty -> new_node Empty a Empty
      | Node (_, l, v, r) when compare a v < 0 -> avl_balance (add l a) v r
      | Node (_, l, v, r) when compare a v > 0 -> avl_balance l v (add r a)
      | Node (h, l, _, r) -> Node (h, l, a, r)
    ;;

    (* When the element is not in the tree, this function simple does nothing. *)
    let rec remove tree a =
      match tree with
      | Empty -> Empty
      | Node (_, l, v, r) when compare a v < 0 -> avl_balance (remove l a) v r
      | Node (_, l, v, r) when compare a v > 0 -> avl_balance l v (remove r a)
      | Node (_, l, _, r) -> merge l r
    ;;

    (* Use a custom key to identify an entry and update it using a function,
       this function must not change the ordering of the entry. *)
    let rec update tree k ~compare ~f =
      let update' = update ~compare ~f in
      match tree with
      | Empty -> Empty
      | Node (h, l, v, r) when compare k v < 0 -> Node (h, update' l k, v, r)
      | Node (h, l, v, r) when compare k v > 0 -> Node (h, l, v, update' r k)
      | Node (h, l, v, r) ->
        let v' = f v in
        assert (E.compare v v' = 0);
        Node (h, l, v', r)
    ;;

    let is_empty = function
      | Empty -> true
      | _ -> false
    ;;

    let rec min_value = function
      | Empty -> None
      | Node (_, Empty, v, _) -> Some v
      | Node (_, l, _, _) -> min_value l
    ;;

    let rec max_value = function
      | Empty -> None
      | Node (_, _, v, Empty) -> Some v
      | Node (_, _, _, r) -> max_value r
    ;;

    (* Finds the largest value that is smaller than e in the tree *)
    let pred tree e =
      let rec pred' tree e acc =
        match tree with
        | Empty -> acc
        | Node (_, l, v, _) when compare e v < 0 -> pred' l e acc
        | Node (_, _, v, r) when compare e v > 0 -> pred' r e (Some v)
        | Node (_, l, _, _) ->
          (match max_value l with
           | None -> acc
           | Some ml -> Some ml)
      in
      pred' tree e None
    ;;

    (* Finds the smallest value that is larger than e in the tree *)
    let succ tree e =
      let rec succ' tree e acc =
        match tree with
        | Empty -> acc
        | Node (_, l, v, _) when compare e v < 0 -> succ' l e (Some v)
        | Node (_, _, v, r) when compare e v > 0 -> succ' r e acc
        | Node (_, l, _, _) ->
          (match min_value l with
           | None -> acc
           | Some ml -> Some ml)
      in
      succ' tree e None
    ;;

    (* Looks for a certain entry using a custom compare function and splits
       it off from the tree. Essentially combines remove and find. *)
    let rec split_cmp tree ~compare i =
      match tree with
      | Empty -> None
      | Node (_, l, j, r) when compare i j < 0 ->
        split_cmp l ~compare i |> Option.map (fun (l', y) -> avl_balance l' j r, y)
      | Node (_, l, j, r) when compare i j > 0 ->
        split_cmp r ~compare i |> Option.map (fun (r', y) -> avl_balance l j r', y)
      | Node (_, l, j, r) -> Some (merge l r, j)
    ;;

    let split = split_cmp ~compare:E.compare

    let rec split_min tree =
      match tree with
      | Empty -> None
      | Node (_, Empty, x, r) -> Some (r, x)
      | Node (_, l, x, r) ->
        let l', y = split_min l |> Option.get in
        Some (avl_balance l' x r, y)
    ;;

    let traverse tree =
      let rec traverse' tree acc =
        match tree with
        | Empty -> acc
        | Node (_, l, v, r) -> traverse' l (v :: traverse' r acc)
      in
      traverse' tree []
    ;;
  end
end
