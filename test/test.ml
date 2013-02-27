open OUnit

(*
  properties:

  - equal: has the same members

  - forall t i j x y. i <> j -> add i x (add j y t) = add j y (add i x t)
  - forall t i x y. add i x (add i y t) = add i x t
  - forall t i x. remove i (add i x t) = t
  - forall i. remove i empty = empty
  - forall t i j x. i <> j -> remove i (add j x t) = add j x (remove i t)
  - forall i x. singleton i x = add i x empty

  - forall i x t. iter (fun j y -> 

*)

let to_assoc_list t =
  List.sort (fun (i,_) (j,_) -> compare i j)
    (ArrayMappedTrie.fold (fun l i x -> (i,x)::l) [] t)

let equal t1 t2 =
  to_assoc_list t1 = to_assoc_list t2

(******************************************************************************)
let test_empty_is_empty () =
  assert_bool
    "empty_is_empty"
    (ArrayMappedTrie.is_empty ArrayMappedTrie.empty)

let test_insert () =
  assert_equal
    (ArrayMappedTrie.find 12 (ArrayMappedTrie.add 12 'a' ArrayMappedTrie.empty))
    'a'

(******************************************************************************)
let suite =
  "hashtree tests" >:::
    [ "empty_is_empty" >:: test_empty_is_empty
    ; "insert"         >:: test_insert
    ]

let _ =
  Random.self_init ();
  run_test_tt_main suite
