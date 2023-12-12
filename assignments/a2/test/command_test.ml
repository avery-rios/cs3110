open OUnit2
open Adventure.Command

let test_cmd name input output =
  name >:: fun _ -> assert_equal output (parse input)

let go_test =
  "go command"
  >::: [
         test_cmd "go north" "go north" (CGo "north");
         test_cmd "go NOrth" "go NOrth" (CGo "north");
         test_cmd "north without go" "north" (CGo "north");
         test_cmd "with space" " go NorTH     " (CGo "north");
         test_cmd "multi word" "  go clock tower " (CGo "clock tower");
         test_cmd "space between" "go    clock tower" (CGo "clock tower");
       ]

let inv_test =
  "inventory"
  >::: [
         test_cmd "inv shorthand" "inv" CInventory;
         test_cmd "inventory" "inventory" CInventory;
       ]

let items_test =
  "take and drop"
  >::: [
         test_cmd "take" "take key" (CTake "key");
         test_cmd "drop" "drop key" (CDrop "key");
         test_cmd "multi word take" "   take some key   " (CTake "some key");
         test_cmd "multi word drop" "   drop some key   " (CDrop "some key");
         test_cmd "space between" "take     some key   " (CTake "some key");
       ]

let test_suites =
  "command"
  >::: [
         test_cmd "quit" "quit" CQuit;
         test_cmd "look" "look" CLook;
         test_cmd "with space" "quit   " CQuit;
         test_cmd "upper case" "QUiT" CQuit;
         go_test;
         items_test;
         inv_test;
       ]
