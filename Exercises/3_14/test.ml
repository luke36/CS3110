open OUnit2
open E_3_14

let make_product_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (product input) ~printer:string_of_int

let product_tests =
  "test suite for product"
  >::: [
         make_product_test "empty" 1 [];
         make_product_test "singleton" 2 [ 2 ];
         make_product_test "multiple" 280 [ 2; 5; 7; 4 ];
       ]

let take_5th_tests =
  "test suite for take_5th"
  >::: [
         ("empty" >:: fun _ -> assert_equal 0 (take_5th []));
         ("less than 5" >:: fun _ -> assert_equal 0 (take_5th [ 1; 2; 3 ]));
         ( "more than 5" >:: fun _ ->
           assert_equal 5 (take_5th [ 1; 2; 3; 4; 5; 6 ]) );
       ]

let sort_descend_tests =
  "test suite for sort_descend"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (sort_descend []));
         ( "multiple" >:: fun _ ->
           assert_equal [ 6; 4; 4; 3; 2; 1 ] (sort_descend [ 4; 3; 1; 6; 2; 4 ])
         );
       ]

let list_max_tests =
  "test list max"
  >::: [
         ( "empty" >:: fun _ ->
           assert_raises (Failure "list_max") (fun () -> list_max []) );
         ("singleton" >:: fun _ -> assert_equal 1 (list_max [ 1 ]));
         ("multiple" >:: fun _ -> assert_equal 3 (list_max [ 1; 2; 3 ]));
       ]

let () =
  run_test_tt_main product_tests;
  run_test_tt_main take_5th_tests;
  run_test_tt_main sort_descend_tests;
  run_test_tt_main list_max_tests
