open OUnit

(******************************************************************************)
let test_empty_is_empty () =
  assert_bool
    "empty_is_empty"
    (Hashtree.is_empty Hashtree.empty)

(******************************************************************************)
let suite =
  "hashtree tests" >:::
    [ "empty_is_empty" >:: test_empty_is_empty
    ]

let _ =
  Random.self_init ();
  run_test_tt_main suite
