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
          ( Array.const (Array.mk_type (Array.mk_type int_t))
          @@ [|
               Array.const (Array.mk_type int_t)
                 Int.[| int 3; int 7; int 5; int 2; int 8 |];
             |] ) );
    ]

  let output =
    ( "L",
      Value.A
        ( Array.const (Array.mk_type (Array.mk_type int_t))
        @@ [|
             Array.const (Array.mk_type int_t)
               Int.[| int 3; int 2; int 5; int 2; int 3 |];
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

    #include "sexp.hpp"
    int reconstruct_L_1(const std::vector<std::vector<int>> &x15);
    int main();
    std::vector<int> x2(5);
    std::vector<std::vector<int>> x3(1);
    std::vector<int> x4(5);
    std::vector<std::vector<int>> x5(1);
    std::vector<std::set<std::pair<std::vector<int>, std::vector<int>>>> x6;
    std::set<std::pair<std::vector<int>, std::vector<int>>> x8;
    std::vector<std::set<std::pair<std::vector<int>, std::vector<int>>>> x9;
    std::vector<
        std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>>>
        x10;
    std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>> x12;
    std::vector<
        std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>>>
        x13;
    int x19;
    std::vector<int> x20(0);
    int main() {
      // begin Array.init
      x6.clear();
      x6.reserve(100);
      for (int x7 = 0; x7 < 100; x7 += 1) {
        x6.push_back(x8);
      }
      // end Array.init
      x9 = x6;
      // begin Array.init
      x10.clear();
      x10.reserve(100);
      for (int x11 = 0; x11 < 100; x11 += 1) {
        x10.push_back(x12);
      }
      // end Array.init
      x13 = x10;
      // begin Array.const

      // begin Array.const
      x2[0] = 3;
      x2[1] = 7;
      x2[2] = 5;
      x2[3] = 2;
      x2[4] = 8;  // end Array.const
      x3[0] = x2; // end Array.const
      std::vector<std::vector<int>> x14 = x3;
      // begin Array.const

      // begin Array.const
      x4[0] = 3;
      x4[1] = 2;
      x4[2] = 5;
      x4[3] = 2;
      x4[4] = 3;  // end Array.const
      x5[0] = x4; // end Array.const

      if ((x14 == x5)) {
        std::cout << "Starting reconstruction" << std::endl;
        // begin Array.const

        // begin Array.const
        x4[0] = 3;
        x4[1] = 2;
        x4[2] = 5;
        x4[3] = 2;
        x4[4] = 3;  // end Array.const
        x5[0] = x4; // end Array.const
        reconstruct_L_1(x5);
        int x18 = 0;
        exit(0);
        x19 = 0;
      } else {

        x19 = 0;
      }
      std::cout << "Inserting (L -> i0) cost 1" << std::endl;
      // begin Array.const
      // end Array.const
      (x13[1]).insert(std::make_pair(x14, x20));
      return 0;
    }
    int reconstruct_L_1(const std::vector<std::vector<int>> &x15) {
      int x17;
      std::vector<std::vector<int>> x16 = x15;
      // begin Array.const

      // begin Array.const
      x2[0] = 3;
      x2[1] = 7;
      x2[2] = 5;
      x2[3] = 2;
      x2[4] = 8;  // end Array.const
      x3[0] = x2; // end Array.const

      if ((0 || (x3 == x16))) {
        std::cout << "(App i0 ())" << std::endl;
        x17 = 0;
      } else {

        x17 = 0;
      }
      return 0;
    }
 |}]
