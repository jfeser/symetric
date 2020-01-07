open! Core

module Code = Cstage.Code ()

module Deepcoder = Deepcoder.Make (Code)

module Examples = struct
  open Code
  open Deepcoder

  let inputs =
    [
      ( "L",
        Value.A
          ( Array.const (Array.mk_type (Array.mk_type Int))
          @@ [|
               Array.const (Array.mk_type Int)
                 [| int 3; int 7; int 5; int 2; int 8 |];
             |] ) );
    ]

  let output =
    ( "L",
      Value.A
        ( Array.const (Array.mk_type (Array.mk_type Int))
        @@ [|
             Array.const (Array.mk_type Int)
               [| int 3; int 2; int 5; int 2; int 3 |];
           |] ) )
end

module DeepSynth =
  Synth.Make (Code) (Deepcoder.Lang (Examples)) (Deepcoder.Cache)

let%expect_test "" =
  DeepSynth.enumerate 1 |> Code.to_string |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int main();
    std::vector<int> x0(5);
    std::vector<std::vector<int>> x1(1);
    std::vector<int> x2(5);
    std::vector<std::vector<int>> x3(1);
    std::vector<std::set<std::pair<std::vector<int>, std::vector<int>>>> x4;
    std::set<std::pair<std::vector<int>, std::vector<int>>> x6;
    std::vector<
        std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>>>
        x7;
    std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>> x9;
    int main() {
      // begin Array.init
      x4.clear();
      x4.reserve(100);
      for (int x5 = 0; x5 < 100; x5++) {
        x4.push_back(x6);
      }
      // end Array.init

      // begin Array.init
      x7.clear();
      x7.reserve(100);
      for (int x8 = 0; x8 < 100; x8++) {
        x7.push_back(x9);
      }
      // end Array.init
      return 0;
    }
 |}]
