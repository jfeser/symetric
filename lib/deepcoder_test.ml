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

module Sketch = struct
  let inputs = [ "L" ]

  let output = "L"
end

module DeepSynth = Synth.Make (Sketch) (Code) (Deepcoder.Lang) (Deepcoder.Cache)

let%expect_test "" =
  DeepSynth.enumerate 2 |> Code.to_string |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>

    #include "sexp.hpp"
    int reconstruct_L_1(const std::vector<std::vector<int>> &x44);
    int reconstruct_L_2(const std::vector<std::vector<int>> &x33);
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
    std::vector<std::vector<int>> x16;
    std::vector<int> x20;
    std::vector<int> x24(0);
    std::vector<std::vector<int>> x26;
    std::vector<int> x29;
    int x47;
    std::vector<std::vector<int>> x49;
    std::vector<int> x52;
    std::vector<int> x55(0);
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
      const std::vector<std::unique_ptr<sexp>> &x48 =
          ((list *)sexp::load(std::cin).get())->get_body();
      // begin Array.init
      x49.clear();
      x49.reserve((int)((x48).size()));
      for (int x50 = 0; x50 < (int)((x48).size()); x50 += 1) {
        const std::vector<std::unique_ptr<sexp>> &x51 =
            ((list *)(x48)[x50].get())->get_body();
        // begin Array.init
        x52.clear();
        x52.reserve((int)((x51).size()));
        for (int x53 = 0; x53 < (int)((x51).size()); x53 += 1) {
          int x54 = std::stoi(((atom *)(x51)[x53].get())->get_body());
          x52.push_back(x54);
        }
        // end Array.init
        x49.push_back(x52);
      }
      // end Array.init

      // begin Array.const
      // end Array.const
      (x13[1]).insert(std::make_pair(x49, x55));
      const std::vector<std::unique_ptr<sexp>> &x25 =
          ((list *)sexp::load(std::cin).get())->get_body();
      // begin Array.init
      x26.clear();
      x26.reserve((int)((x25).size()));
      for (int x27 = 0; x27 < (int)((x25).size()); x27 += 1) {
        const std::vector<std::unique_ptr<sexp>> &x28 =
            ((list *)(x25)[x27].get())->get_body();
        // begin Array.init
        x29.clear();
        x29.reserve((int)((x28).size()));
        for (int x30 = 0; x30 < (int)((x28).size()); x30 += 1) {
          int x31 = std::stoi(((atom *)(x28)[x30].get())->get_body());
          x29.push_back(x31);
        }
        // end Array.init
        x26.push_back(x29);
      }
      // end Array.init
      std::vector<std::vector<int>> x32 = x26;
      for (auto x14 = (x13[2]).begin(); x14 != (x13[2]).end(); ++x14) {
        std::vector<std::vector<int>> x15 = std::get<0>(*x14);
        // begin Array.init
        x16.clear();
        x16.reserve(((int)((x15).size())));
        for (int x17 = 0; x17 < ((int)((x15).size())); x17 += 1) {
          std::vector<int> x18 = (x15[x17]);
          int x19 = ((int)((x18).size()));
          // begin Array.init
          x20.clear();
          x20.reserve(x19);
          for (int x21 = 0; x21 < x19; x21 += 1) {
            int x22 = (x18[((x19 - x21) - 1)]);
            x20.push_back(x22);
          }
          // end Array.init
          x16.push_back(x20);
        }
        // end Array.init
        std::vector<std::vector<int>> x23 = x16;
        if ((x23 == x32)) {
          std::cout << "Starting reconstruction" << std::endl;
          reconstruct_L_2(x32);
          exit(0);
          x47 = 0;
        } else {

          x47 = 0;
        }
        std::cout << "Inserting (L -> reverse(L)) cost 2" << std::endl;
        // begin Array.const
        // end Array.const
        (x13[2]).insert(std::make_pair(x23, x24));
      }
      return 0;
    }
    int reconstruct_L_2(const std::vector<std::vector<int>> &x33) {
      std::vector<std::vector<int>> x37;
      std::vector<int> x41;
      int x46;
      std::vector<std::vector<int>> x34 = x33;
      for (auto x35 = (x13[1]).begin(); x35 != (x13[1]).end(); ++x35) {
        std::vector<std::vector<int>> x36 = std::get<0>(*x35);
        // begin Array.init
        x37.clear();
        x37.reserve(((int)((x36).size())));
        for (int x38 = 0; x38 < ((int)((x36).size())); x38 += 1) {
          std::vector<int> x39 = (x36[x38]);
          int x40 = ((int)((x39).size()));
          // begin Array.init
          x41.clear();
          x41.reserve(x40);
          for (int x42 = 0; x42 < x40; x42 += 1) {
            int x43 = (x39[((x40 - x42) - 1)]);
            x41.push_back(x43);
          }
          // end Array.init
          x37.push_back(x41);
        }
        // end Array.init

        if ((0 || (x37 == x34))) {
          std::cout << "(App reverse ((App L0 ())))" << std::endl;
          reconstruct_L_1(std::get<0>(*x35));
          x46 = 0;
        } else {

          x46 = 0;
        }
      }
      return 0;
    }
    int reconstruct_L_1(const std::vector<std::vector<int>> &x44) {
      std::vector<std::vector<int>> x45 = x44;
      return 0;
    }
 |}]
