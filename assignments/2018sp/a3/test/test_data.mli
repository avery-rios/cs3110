(* [Tests] is the output type of the testing functor below. *)
module type Tests = sig
  (* A list of OUnit tests. *)
  val tests : OUnit2.test list
end

(* [DictTester] takes a [DictionaryMaker], uses it to make
 * a dictionary, and returns OUnit tests for that dictionary. *)
module DictTester (_ : Search.Data.DictionaryMaker) : Tests

(* [tests] contains the test cases for data structure
 * implementations in [Data]:
 *  - dictionaries represented as association lists
 *  - dictionaries representd as 2-3 trees
 *  - sets as list-represented dictionaries
 *  - sets as tree-represented dictionaries
 * It would make sense to use [DictTester] to produces
 * the first two of those four test groups. *)
val tests : OUnit2.test list
