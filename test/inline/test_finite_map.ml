open Base
open Fdsa.Finite_map
open Fdsa.List_utils
module IntMap = FiniteMap.Make (Int) (String)

let base_map =
  List.range ?stop:(Some `inclusive) 1 10
  |> shuffle
  |> List.fold ~f:(fun m i -> IntMap.add m i (Int.to_string i)) ~init:IntMap.empty
;;

let remove_multiple map keys =
  List.fold ~init:map ~f:(fun map k -> IntMap.remove map k) keys
;;

let kv_fmt = Fmt.(list ~sep:semi @@ pair ~sep:comma int string)
let%test_unit "empty map is empty" = assert (IntMap.is_empty IntMap.empty)
let%test_unit "base map is not empty" = assert (not @@ IntMap.is_empty base_map)

let%expect_test "print all kv pairs" =
  Fmt.pr "%a" Fmt.(list ~sep:semi @@ pair ~sep:comma int string) (IntMap.to_list base_map);
  [%expect {|
    1, 1; 2, 2; 3, 3; 4, 4; 5, 5; 6, 6; 7, 7; 8, 8; 9, 9; 10,
    10
    |}]
;;

let%expect_test "five squared" =
  let square_map = IntMap.update base_map 5 ~f:(fun k _ -> Int.to_string (k * k)) in
  Fmt.pr "%a" kv_fmt (IntMap.to_list square_map);
  [%expect
    {|
    1, 1; 2, 2; 3, 3; 4, 4; 5, 25; 6, 6; 7, 7; 8, 8; 9, 9; 10,
    10
    |}]
;;

let%expect_test "remove absent key" =
  Fmt.pr "%a" kv_fmt (IntMap.to_list @@ IntMap.remove base_map 0);
  [%expect
    {|
    1, 1; 2, 2; 3, 3; 4, 4; 5, 5; 6, 6; 7, 7; 8, 8; 9, 9; 10,
    10
    |}]
;;

let%expect_test "remove one by one" =
  List.range ?stop:(Some `inclusive) 1 10
  |> List.iter ~f:(fun k ->
    IntMap.remove base_map k |> IntMap.to_list |> Fmt.pr "%a@." kv_fmt);
  [%expect
    {|
    2, 2; 3, 3; 4, 4; 5, 5; 6, 6; 7, 7; 8, 8; 9, 9; 10,
    10
    1, 1; 3, 3; 4, 4; 5, 5; 6, 6; 7, 7; 8, 8; 9, 9; 10,
    10
    1, 1; 2, 2; 4, 4; 5, 5; 6, 6; 7, 7; 8, 8; 9, 9; 10,
    10
    1, 1; 2, 2; 3, 3; 5, 5; 6, 6; 7, 7; 8, 8; 9, 9; 10,
    10
    1, 1; 2, 2; 3, 3; 4, 4; 6, 6; 7, 7; 8, 8; 9, 9; 10,
    10
    1, 1; 2, 2; 3, 3; 4, 4; 5, 5; 7, 7; 8, 8; 9, 9; 10,
    10
    1, 1; 2, 2; 3, 3; 4, 4; 5, 5; 6, 6; 8, 8; 9, 9; 10,
    10
    1, 1; 2, 2; 3, 3; 4, 4; 5, 5; 6, 6; 7, 7; 9, 9; 10,
    10
    1, 1; 2, 2; 3, 3; 4, 4; 5, 5; 6, 6; 7, 7; 8, 8; 10,
    10
    1, 1; 2, 2; 3, 3; 4, 4; 5, 5; 6, 6; 7, 7; 8, 8; 9,
    9
    |}]
;;

let%expect_test "remove some" =
  let to_remove = [
    [1;2;3];
    [4;5;6];
    [7;8;9;10];
    [9;4;7;1];
    [8;10;2;3;8];
  ] in
  List.iter
    ~f:(fun keys ->
      remove_multiple base_map keys |> IntMap.to_list |> Fmt.pr "%a@." kv_fmt)
    to_remove;
  [%expect
    {|
    4, 4; 5, 5; 6, 6; 7, 7; 8, 8; 9, 9; 10,
    10
    1, 1; 2, 2; 3, 3; 7, 7; 8, 8; 9, 9; 10,
    10
    1, 1; 2, 2; 3, 3; 4, 4; 5, 5; 6,
    6
    2, 2; 3, 3; 5, 5; 6, 6; 8, 8; 10,
    10
    1, 1; 4, 4; 5, 5; 6, 6; 7, 7; 9,
    9
    |}]
;;

let%test_unit "remove all" =
  let empty = List.range ?stop:(Some `inclusive) 1 10 |> remove_multiple base_map in
  assert (IntMap.is_empty empty)
;;
