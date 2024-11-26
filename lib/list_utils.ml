open Base

let rec shuffle = function
  | [] -> []
  | [ single ] -> [ single ]
  | list ->
    let before, after = List.partition_tf ~f:(fun _ -> Random.bool ()) list in
    List.rev_append (shuffle before) (shuffle after)
;;
