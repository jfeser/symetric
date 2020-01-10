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
                 [| int 3; int 7; int 5; int 2; int 8 |];
             |] ) );
    ]

  let output =
    ( "L",
      Value.A
        ( Array.const (Array.mk_type (Array.mk_type int_t))
        @@ [|
             Array.const (Array.mk_type int_t)
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
    int reconstruct_L_1(const std::vector<std::vector<int>> &x13);
    int main();
    std::vector<int> x0(5);
    std::vector<std::vector<int>> x1(1);
    std::vector<int> x2(5);
    std::vector<std::vector<int>> x3(1);
    std::vector<std::set<std::pair<std::vector<int>, std::vector<int>>>> x4;
    std::set<std::pair<std::vector<int>, std::vector<int>>> x6;
    std::vector<std::set<std::pair<std::vector<int>, std::vector<int>>>> x7;
    std::vector<
        std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>>>
        x8;
    std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>> x10;
    std::vector<
        std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>>>
        x11;
    int x17;
    std::vector<int> x18(0);
    int main() {
      // begin Array.init
      x4.clear();
      x4.reserve(100);
      for (int x5 = 0; x5 < 100; x5 += 1) {
        x4.push_back(x6);
      }
      // end Array.init
      x7 = x4;
      // begin Array.init
      x8.clear();
      x8.reserve(100);
      for (int x9 = 0; x9 < 100; x9 += 1) {
        x8.push_back(x10);
      }
      // end Array.init
      x11 = x8;
      // begin Array.const

      // begin Array.const
      x0[0] = 3;
      x0[1] = 7;
      x0[2] = 5;
      x0[3] = 2;
      x0[4] = 8;  // end Array.const
      x1[0] = x0; // end Array.const
      std::vector<std::vector<int>> x12 = x1;
      // begin Array.const

      // begin Array.const
      x2[0] = 3;
      x2[1] = 2;
      x2[2] = 5;
      x2[3] = 2;
      x2[4] = 3;  // end Array.const
      x3[0] = x2; // end Array.const

      if ((x12 == x3)) {
        std::cout << "Starting reconstruction" << std::endl;
        // begin Array.const

        // begin Array.const
        x2[0] = 3;
        x2[1] = 2;
        x2[2] = 5;
        x2[3] = 2;
        x2[4] = 3;  // end Array.const
        x3[0] = x2; // end Array.const
        reconstruct_L_1(x3);
        int x16 = 0;
        exit(0);
        x17 = 0;
      } else {

        x17 = 0;
      }
      std::cout << "Inserting (L -> i0) cost 1" << std::endl;
      // begin Array.const
      // end Array.const
      (x11[1]).insert(std::make_pair(x12, x18));
      return 0;
    }
    int reconstruct_L_1(const std::vector<std::vector<int>> &x13) {
      int x15;
      std::vector<std::vector<int>> x14 = x13;
      // begin Array.const

      // begin Array.const
      x0[0] = 3;
      x0[1] = 7;
      x0[2] = 5;
      x0[3] = 2;
      x0[4] = 8;  // end Array.const
      x1[0] = x0; // end Array.const

      if ((0 || (x1 == x14))) {
        std::cout << "(App i0 ())" << std::endl;
        x15 = 0;
      } else {

        x15 = 0;
      }
      return 0;
    }
 |}]
