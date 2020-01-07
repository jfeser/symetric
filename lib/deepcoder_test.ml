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
    int reconstruct_L_1(std::vector<std::vector<int>> &x10);
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
    int x11;
    std::vector<int> x12(0);
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

      // begin Array.const

      // begin Array.const
      x0[0] = 3;
      x0[1] = 7;
      x0[2] = 5;
      x0[3] = 2;
      x0[4] = 8;  // end Array.const
      x1[0] = x0; // end Array.const

      // begin Array.const

      // begin Array.const
      x2[0] = 3;
      x2[1] = 2;
      x2[2] = 5;
      x2[3] = 2;
      x2[4] = 3;  // end Array.const
      x3[0] = x2; // end Array.const

      if ((x1 == x3)) {
        std::cout << "Starting reconstruction" << std::endl;
        // begin Array.const

        // begin Array.const
        x2[0] = 3;
        x2[1] = 2;
        x2[2] = 5;
        x2[3] = 2;
        x2[4] = 3;  // end Array.const
        x3[0] = x2; // end Array.const
        exit(0);
        x11 = 0;
      } else {

        x11 = 0;
      }
      std::cout << "Inserting (L -> i0) cost 1" << std::endl;
      // begin Array.const

      // begin Array.const
      x0[0] = 3;
      x0[1] = 7;
      x0[2] = 5;
      x0[3] = 2;
      x0[4] = 8;  // end Array.const
      x1[0] = x0; // end Array.const

      // begin Array.const
      // end Array.const
      (x7[1]).insert(std::make_pair(x1, x12));
      return 0;
    }
    int reconstruct_L_1(std::vector<std::vector<int>> &x10) { return 0; }
 |}]
