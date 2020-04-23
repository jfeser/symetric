open! Core

let%expect_test "" =
  let module Core = Cstage_core.Make () in
  let module Code = Cstage.Code (Core) in
  let module Array = Cstage_array.Array (Core) in
  let module Deepcoder = Deepcoder.Make (Array) (Code) in
  let module Sketch = struct
    let inputs = [ "L" ]

    let output = "L"
  end in
  let module DeepSynth =
    Synth.Make (Sketch) (Code) (Deepcoder.Lang) (Deepcoder.Cache)
  in
  let code = DeepSynth.enumerate 2 |> Code.to_string in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <array>
    #include <cmath>
    #include <iostream>
    #include <set>
    #include <vector>

    #include "sexp.hpp"
    #include "stdlib.hpp"

    template <typename T> struct span {
      T *ptr;
      int len;

      bool operator==(const span<T> &rhs) const {
        if (len != rhs.len) {
          return false;
        } else {
          bool ret = true;
          for (int i = 0; i < len; i++) {
            ret = ret && (ptr[i] == rhs.ptr[i]);
          }
          return ret;
        }
      }

      bool operator<(const span<T> &rhs) const {
        if (len != rhs.len) {
          return len < rhs.len;
        }
        for (int i = 0; i < len; i++) {
          if (ptr[i] != rhs.ptr[i]) {
            return ptr[i] < rhs.ptr[i];
          }
        }
        return false;
      }

      bool operator<=(const span<T> &rhs) const {
        return this < rhs || this == rhs;
      }
      bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
      bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
    };

    int reconstruct_L_1(
        std::pair<
            std::pair<std::vector<std::set<std::vector<int32_t>>>,
                      std::vector<std::set<std::vector<std::vector<int32_t>>>>>,
            std::vector<std::vector<int32_t>>>
            x95);
    int reconstruct_L_2(
        std::pair<
            std::pair<std::vector<std::set<std::vector<int32_t>>>,
                      std::vector<std::set<std::vector<std::vector<int32_t>>>>>,
            std::vector<std::vector<int32_t>>>
            x76);
    int main();
    std::vector<sexp *> x15;
    std::vector<std::vector<std::vector<int32_t>>> x45;
    std::vector<std::vector<int32_t>> x75;
    int main() {
      std::set<std::vector<std::vector<int32_t>>> x2;
      std::set<std::vector<std::vector<int32_t>>> x6;
      std::set<std::vector<int32_t>> x8;
      std::set<std::vector<int32_t>> x12;
      // begin Array.init
      std::vector<std::set<std::vector<int32_t>>> x9(100);
      std::vector<std::set<std::vector<int32_t>>> x10 = x9;
      for (int x11 = 0; x11 < 100; x11 += 1) {
        x10[x11] = x12;
      }
      // end Array.init
      std::vector<std::set<std::vector<int32_t>>> x13 = x10;
      // begin Array.init
      std::vector<std::set<std::vector<std::vector<int32_t>>>> x3(100);
      std::vector<std::set<std::vector<std::vector<int32_t>>>> x4 = x3;
      for (int x5 = 0; x5 < 100; x5 += 1) {
        x4[x5] = x6;
      }
      // end Array.init
      std::vector<std::set<std::vector<std::vector<int32_t>>>> x7 = x4;
      sexp *x14 = sexp::load(std::cin);
      x15 = ((list *)x14)->get_body();
      std::vector<sexp *> x63 = ((list *)(x15)[1])->get_body();
      // begin Array.init
      std::vector<std::vector<int32_t>> x68((int)((x63).size()));
      std::vector<std::vector<int32_t>> x69 = x68;
      for (int x70 = 0; x70 < (int)((x63).size()); x70 += 1) {
        std::vector<sexp *> x71 = ((list *)(x63)[x70])->get_body();
        // begin Array.init
        std::vector<int32_t> x72((int)((x71).size()));
        std::vector<int32_t> x73 = x72;
        for (int x74 = 0; x74 < (int)((x71).size()); x74 += 1) {
          x73[x74] = std::stoi(((atom *)(x71)[x74])->get_body());
        }
        // end Array.init
        x69[x70] = x73;
      }
      // end Array.init
      x75 = x69;
      std::vector<sexp *> x16 = ((list *)(x15)[0])->get_body();
      int x17 = (int)((x16).size());
      // begin Array.init
      std::vector<std::vector<std::vector<int32_t>>> x30(x17);
      std::vector<std::vector<std::vector<int32_t>>> x31 = x30;
      for (int x32 = 0; x32 < x17; x32 += 1) {
        std::vector<sexp *> x33 = ((list *)(x16)[x32])->get_body();
        // begin Array.init
        std::vector<std::vector<int32_t>> x38((int)((x33).size()));
        std::vector<std::vector<int32_t>> x39 = x38;
        for (int x40 = 0; x40 < (int)((x33).size()); x40 += 1) {
          std::vector<sexp *> x41 = ((list *)(x33)[x40])->get_body();
          // begin Array.init
          std::vector<int32_t> x42((int)((x41).size()));
          std::vector<int32_t> x43 = x42;
          for (int x44 = 0; x44 < (int)((x41).size()); x44 += 1) {
            x43[x44] = std::stoi(((atom *)(x41)[x44])->get_body());
          }
          // end Array.init
          x39[x40] = x43;
        }
        // end Array.init
        x31[x32] = x39;
      }
      // end Array.init
      x45 = x31;
      // begin Array.iter
      int32_t x46 = ((int)((x45).size()));
      for (int x47 = 0; x47 < x46; x47 += 1) {
        std::vector<std::vector<int32_t>> x48 = (x45[x47]);
        // begin Array.init
        std::vector<std::vector<int32_t>> x54(((int)((x48).size())));
        std::vector<std::vector<int32_t>> x55 = x54;
        for (int x56 = 0; x56 < ((int)((x48).size())); x56 += 1) {
          std::vector<int32_t> x57 = (x48[x56]);
          int32_t x58 = ((int)((x57).size()));
          // begin Array.init
          std::vector<int32_t> x59(x58);
          std::vector<int32_t> x60 = x59;
          for (int x61 = 0; x61 < x58; x61 += 1) {
            x60[x61] = (x57[((x58 - x61) - 1)]);
          }
          // end Array.init
          x55[x56] = x60;
        }
        // end Array.init
        std::vector<std::vector<int32_t>> x62 = x55;
        int x100;
        int x101 = x100;
        if ((x62 == x75)) {
          reconstruct_L_2(std::make_pair(std::make_pair(x13, x7), x75));
          exit(0);
          x101 = 0;
        } else {

          x101 = 0;
        }
        (x7[2]).insert(x62);
      }
      // end Array.iter
      return 0;
    }
    int reconstruct_L_2(
        std::pair<
            std::pair<std::vector<std::set<std::vector<int32_t>>>,
                      std::vector<std::set<std::vector<std::vector<int32_t>>>>>,
            std::vector<std::vector<int32_t>>>
            x76) {
      std::pair<std::vector<std::set<std::vector<int32_t>>>,
                std::vector<std::set<std::vector<std::vector<int32_t>>>>>
          x77 = std::get<0>(x76);
      std::vector<std::vector<int32_t>> x78 = std::get<1>(x76);
      // begin Array.iter
      int32_t x79 = ((int)((x45).size()));
      for (int x80 = 0; x80 < x79; x80 += 1) {
        int x98;
        int x99 = x98;
        std::vector<std::vector<int32_t>> x81 = (x45[x80]);
        // begin Array.init
        std::vector<std::vector<int32_t>> x87(((int)((x81).size())));
        std::vector<std::vector<int32_t>> x88 = x87;
        for (int x89 = 0; x89 < ((int)((x81).size())); x89 += 1) {
          std::vector<int32_t> x90 = (x81[x89]);
          int32_t x91 = ((int)((x90).size()));
          // begin Array.init
          std::vector<int32_t> x92(x91);
          std::vector<int32_t> x93 = x92;
          for (int x94 = 0; x94 < x91; x94 += 1) {
            x93[x94] = (x90[((x91 - x94) - 1)]);
          }
          // end Array.init
          x88[x89] = x93;
        }
        // end Array.init

        if ((x88 == x78)) {
          std::cout << "(App reverse ((App L0 ())))" << std::endl;
          reconstruct_L_1(std::make_pair(x77, (x45[x80])));
          x99 = 0;
        } else {

          x99 = 0;
        }
      }
      // end Array.iter
      return 0;
    }
    int reconstruct_L_1(
        std::pair<
            std::pair<std::vector<std::set<std::vector<int32_t>>>,
                      std::vector<std::set<std::vector<std::vector<int32_t>>>>>,
            std::vector<std::vector<int32_t>>>
            x95) {
      std::pair<std::vector<std::set<std::vector<int32_t>>>,
                std::vector<std::set<std::vector<std::vector<int32_t>>>>>
          x96 = std::get<0>(x95);
      std::vector<std::vector<int32_t>> x97 = std::get<1>(x95);
      return 0;
    }
 |}];
  code |> Util.clang_build |> print_endline;
  [%expect
    {|
    <stdin>:116:70: warning: variable 'x100' is uninitialized when used here [-Wuninitialized]
      std::vector<std::vector<int32_t>> x62 = x55; int x100;  int x101 = x100;
                                                                         ^~~~
    <stdin>:116:56: note: initialize the variable 'x100' to silence this warning
      std::vector<std::vector<int32_t>> x62 = x55; int x100;  int x101 = x100;
                                                           ^
                                                            = 0
    <stdin>:131:28: warning: variable 'x98' is uninitialized when used here [-Wuninitialized]
           int x98;  int x99 = x98;    std::vector<std::vector<int32_t>> x81 = (x45[x80]);
                               ^~~
    <stdin>:131:15: note: initialize the variable 'x98' to silence this warning
           int x98;  int x99 = x98;    std::vector<std::vector<int32_t>> x81 = (x45[x80]);
                  ^
                   = 0
    2 warnings generated. |}]

let%expect_test "" =
  let module Core = Cstage_core.Make () in
  let module Code = Cstage.Code (Core) in
  let module Array = Cstage_array.ArenaArray (Core) in
  let module Deepcoder = Deepcoder.Make (Array) (Code) in
  let module Sketch = struct
    let inputs = [ "L" ]

    let output = "L"
  end in
  let module DeepSynth =
    Synth.Make (Sketch) (Code) (Deepcoder.Lang) (Deepcoder.Cache)
  in
  let code = DeepSynth.enumerate 2 |> Code.to_string in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <array>
    #include <cmath>
    #include <iostream>
    #include <set>
    #include <vector>

    #include "sexp.hpp"
    #include "stdlib.hpp"

    template <typename T> struct span {
      T *ptr;
      int len;

      bool operator==(const span<T> &rhs) const {
        if (len != rhs.len) {
          return false;
        } else {
          bool ret = true;
          for (int i = 0; i < len; i++) {
            ret = ret && (ptr[i] == rhs.ptr[i]);
          }
          return ret;
        }
      }

      bool operator<(const span<T> &rhs) const {
        if (len != rhs.len) {
          return len < rhs.len;
        }
        for (int i = 0; i < len; i++) {
          if (ptr[i] != rhs.ptr[i]) {
            return ptr[i] < rhs.ptr[i];
          }
        }
        return false;
      }

      bool operator<=(const span<T> &rhs) const {
        return this < rhs || this == rhs;
      }
      bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
      bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
    };

    int reconstruct_L_1(
        std::pair<std::pair<std::vector<std::set<std::vector<int32_t>>>,
                            std::vector<std::set<std::vector<span<int32_t>>>>>,
                  std::vector<span<int32_t>>>
            x119);
    int reconstruct_L_2(
        std::pair<std::pair<std::vector<std::set<std::vector<int32_t>>>,
                            std::vector<std::set<std::vector<span<int32_t>>>>>,
                  std::vector<span<int32_t>>>
            x96);
    int main();
    std::array<int32_t, 500000> x2;
    int32_t x3;
    std::array<int32_t, 500000> x4;
    int32_t x5;
    std::vector<sexp *> x19;
    std::array<int32_t, 500000> x24;
    int32_t x25;
    std::array<int32_t, 500000> x33;
    int32_t x34;
    std::array<int32_t, 500000> x43;
    int32_t x44;
    std::array<int32_t, 500000> x52;
    int32_t x53;
    std::vector<std::vector<span<int32_t>>> x57;
    std::array<int32_t, 500000> x63;
    int32_t x64;
    std::array<int32_t, 500000> x73;
    int32_t x74;
    std::array<int32_t, 500000> x81;
    int32_t x82;
    std::array<int32_t, 500000> x90;
    int32_t x91;
    std::vector<span<int32_t>> x95;
    std::array<int32_t, 500000> x104;
    int32_t x105;
    std::array<int32_t, 500000> x114;
    int32_t x115;
    int main() {
      std::set<std::vector<span<int32_t>>> x6;
      std::set<std::vector<span<int32_t>>> x10;
      std::set<std::vector<int32_t>> x12;
      std::set<std::vector<int32_t>> x16;
      // begin Array.init
      std::vector<std::set<std::vector<int32_t>>> x13(100);
      std::vector<std::set<std::vector<int32_t>>> x14 = x13;
      for (int x15 = 0; x15 < 100; x15 += 1) {
        x14[x15] = x16;
      }
      // end Array.init
      std::vector<std::set<std::vector<int32_t>>> x17 = x14;
      // begin Array.init
      std::vector<std::set<std::vector<span<int32_t>>>> x7(100);
      std::vector<std::set<std::vector<span<int32_t>>>> x8 = x7;
      for (int x9 = 0; x9 < 100; x9 += 1) {
        x8[x9] = x10;
      }
      // end Array.init
      std::vector<std::set<std::vector<span<int32_t>>>> x11 = x8;
      sexp *x18 = sexp::load(std::cin);
      x19 = ((list *)x18)->get_body();
      std::vector<sexp *> x79 = ((list *)(x19)[1])->get_body();
      // begin Array.init
      std::vector<span<int32_t>> x86((int)((x79).size()));
      std::vector<span<int32_t>> x87 = x86;
      for (int x88 = 0; x88 < (int)((x79).size()); x88 += 1) {
        std::vector<sexp *> x89 = ((list *)(x79)[x88])->get_body();
        // begin Array.init
        span<int32_t> x92 =
            (span<int32_t>){(x90).data() + x91, (int)((x89).size())};
        x91 += (int)((x89).size());
        span<int32_t> x93 = x92;
        for (int x94 = 0; x94 < (int)((x89).size()); x94 += 1) {
          x93.ptr[x94] = std::stoi(((atom *)(x89)[x94])->get_body());
        }
        // end Array.init
        x87[x88] = x93;
      }
      // end Array.init
      x95 = x87;
      std::vector<sexp *> x20 = ((list *)(x19)[0])->get_body();
      int x21 = (int)((x20).size());
      // begin Array.init
      std::vector<std::vector<span<int32_t>>> x38(x21);
      std::vector<std::vector<span<int32_t>>> x39 = x38;
      for (int x40 = 0; x40 < x21; x40 += 1) {
        std::vector<sexp *> x41 = ((list *)(x20)[x40])->get_body();
        // begin Array.init
        std::vector<span<int32_t>> x48((int)((x41).size()));
        std::vector<span<int32_t>> x49 = x48;
        for (int x50 = 0; x50 < (int)((x41).size()); x50 += 1) {
          std::vector<sexp *> x51 = ((list *)(x41)[x50])->get_body();
          // begin Array.init
          span<int32_t> x54 =
              (span<int32_t>){(x52).data() + x53, (int)((x51).size())};
          x53 += (int)((x51).size());
          span<int32_t> x55 = x54;
          for (int x56 = 0; x56 < (int)((x51).size()); x56 += 1) {
            x55.ptr[x56] = std::stoi(((atom *)(x51)[x56])->get_body());
          }
          // end Array.init
          x49[x50] = x55;
        }
        // end Array.init
        x39[x40] = x49;
      }
      // end Array.init
      x57 = x39;
      // begin Array.iter
      int32_t x58 = ((int)((x57).size()));
      for (int x59 = 0; x59 < x58; x59 += 1) {
        std::vector<span<int32_t>> x60 = (x57[x59]);
        // begin Array.init
        std::vector<span<int32_t>> x68(((int)((x60).size())));
        std::vector<span<int32_t>> x69 = x68;
        for (int x70 = 0; x70 < ((int)((x60).size())); x70 += 1) {
          span<int32_t> x71 = (x60[x70]);
          int32_t x72 = ((int)((x71).len));
          // begin Array.init
          span<int32_t> x75 = (span<int32_t>){(x73).data() + x74, x72};
          x74 += x72;
          span<int32_t> x76 = x75;
          for (int x77 = 0; x77 < x72; x77 += 1) {
            x76.ptr[x77] = (x71.ptr[((x72 - x77) - 1)]);
          }
          // end Array.init
          x69[x70] = x76;
        }
        // end Array.init
        std::vector<span<int32_t>> x78 = x69;
        int x124;
        int x125 = x124;
        if ((x78 == x95)) {
          reconstruct_L_2(std::make_pair(std::make_pair(x17, x11), x95));
          exit(0);
          x125 = 0;
        } else {

          x125 = 0;
        }
        (x11[2]).insert(x78);
      }
      // end Array.iter
      return 0;
    }
    int reconstruct_L_2(
        std::pair<std::pair<std::vector<std::set<std::vector<int32_t>>>,
                            std::vector<std::set<std::vector<span<int32_t>>>>>,
                  std::vector<span<int32_t>>>
            x96) {
      std::pair<std::vector<std::set<std::vector<int32_t>>>,
                std::vector<std::set<std::vector<span<int32_t>>>>>
          x97 = std::get<0>(x96);
      std::vector<span<int32_t>> x98 = std::get<1>(x96);
      // begin Array.iter
      int32_t x99 = ((int)((x57).size()));
      for (int x100 = 0; x100 < x99; x100 += 1) {
        int x122;
        int x123 = x122;
        std::vector<span<int32_t>> x101 = (x57[x100]);
        // begin Array.init
        std::vector<span<int32_t>> x109(((int)((x101).size())));
        std::vector<span<int32_t>> x110 = x109;
        for (int x111 = 0; x111 < ((int)((x101).size())); x111 += 1) {
          span<int32_t> x112 = (x101[x111]);
          int32_t x113 = ((int)((x112).len));
          // begin Array.init
          span<int32_t> x116 = (span<int32_t>){(x114).data() + x115, x113};
          x115 += x113;
          span<int32_t> x117 = x116;
          for (int x118 = 0; x118 < x113; x118 += 1) {
            x117.ptr[x118] = (x112.ptr[((x113 - x118) - 1)]);
          }
          // end Array.init
          x110[x111] = x117;
        }
        // end Array.init

        if ((x110 == x98)) {
          std::cout << "(App reverse ((App L0 ())))" << std::endl;
          reconstruct_L_1(std::make_pair(x97, (x57[x100])));
          x123 = 0;
        } else {

          x123 = 0;
        }
      }
      // end Array.iter
      return 0;
    }
    int reconstruct_L_1(
        std::pair<std::pair<std::vector<std::set<std::vector<int32_t>>>,
                            std::vector<std::set<std::vector<span<int32_t>>>>>,
                  std::vector<span<int32_t>>>
            x119) {
      std::pair<std::vector<std::set<std::vector<int32_t>>>,
                std::vector<std::set<std::vector<span<int32_t>>>>>
          x120 = std::get<0>(x119);
      std::vector<span<int32_t>> x121 = std::get<1>(x119);
      return 0;
    } |}];
  code |> Util.clang_build |> print_endline;
  [%expect
    {|
    <stdin>:116:63: warning: variable 'x124' is uninitialized when used here [-Wuninitialized]
      std::vector<span<int32_t>> x78 = x69; int x124;  int x125 = x124;
                                                                  ^~~~
    <stdin>:116:49: note: initialize the variable 'x124' to silence this warning
      std::vector<span<int32_t>> x78 = x69; int x124;  int x125 = x124;
                                                    ^
                                                     = 0
    <stdin>:131:30: warning: variable 'x122' is uninitialized when used here [-Wuninitialized]
           int x122;  int x123 = x122;    std::vector<span<int32_t>> x101 = (x57[x100]);
                                 ^~~~
    <stdin>:131:16: note: initialize the variable 'x122' to silence this warning
           int x122;  int x123 = x122;    std::vector<span<int32_t>> x101 = (x57[x100]);
                   ^
                    = 0
    2 warnings generated. |}]

let%expect_test "" =
  let module Code = Mlstage.Code in
  let module Deepcoder = Deepcoder.Make (Code.Array) (Code) in
  let open Deepcoder in
  let g = Grammar.(Rule.of_tuple ("L", Term.app "input" [])) :: Lang.grammar in
  let state = Random.State.make [||] in
  Grammar.sample_seq ~state "L" g
  |> Sequence.filter ~f:(fun t ->
         let n = Grammar.Term.size t in
         6 <= n && n <= 10)
  |> Sequence.filter_map ~f:(fun t ->
         try
           let input = Value.random ~state "L" 5 in
           let input_str = input |> Value.sexp_of |> Code.to_string in

           let output =
             Lang.eval (Map.of_alist_exn (module String) [ ("input", input) ]) t
             |> Value.sexp_of |> Code.to_string
           in
           Some (t, input_str, output)
         with _ -> None)
  |> (fun s -> Sequence.take s 5)
  |> Sequence.iter ~f:(fun (term, input, output) ->
         printf "Term: %s\nInput: %s\nOutput: %s\n\n"
           (Grammar.Term.to_string term)
           input output);
  [%expect
    {|
    Term: map((/4), map((/2), zipwith(max, reverse(input), input)))
    Input: (Sexp ((2 5 9) (8 1 6 2 2 2 5 4) (5 5 1 0 5 0 7) (0 10 1 7 3) (5 4 1 10)))
    Output: (Sexp ((1 0 1) (1 0 0 0 0 0 0 1) (0 0 0 0 0 0 0) (0 1 0 1 0) (1 0 0 1)))

    Term: map((/2), map((+1), map((*3), input)))
    Input: (Sexp ((0 5 2 4 4 6 3 8) (10 5 8 6 3 2 4) () (6 4 9) (2 4 6)))
    Output: (Sexp ((0 8 3 6 6 9 5 12) (15 8 12 9 5 3 6) () (9 6 14) (3 6 9)))

    Term: map((-1), map((/2), map((*4), map((*3), input))))
    Input: (Sexp
     ((4 6 9 3 4 6 10) (9 8 0 6 3) (0 0 2 0 8 9 6 2) (6 9 3 2 3 10 7 3) (4 6 2)))
    Output: (Sexp
     ((23 35 53 17 23 35 59) (53 47 -1 35 17) (-1 -1 11 -1 47 53 35 11)
      (35 53 17 11 17 59 41 17) (23 35 11)))

    Term: zipwith(max, input, map((**2), input))
    Input: (Sexp ((9 10 3 5 1) (1) (9 9 7 10 9 9 3) (0 1) (8 1 9 8 9 5 4 1)))
    Output: (Sexp
     ((81 100 9 25 1) (1) (81 81 49 100 81 81 9) (0 1) (64 1 81 64 81 25 16 1)))

    Term: map((/2), map((*2), map((/4), input)))
    Input: (Sexp ((0 3 5 4 6 0 3 7) (1) () (8 9) (7)))
    Output: (Sexp ((0 0 1 1 1 0 0 1) (0) () (2 2) (1))) |}]
