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
            x49);
    int reconstruct_L_2(
        std::pair<
            std::pair<std::vector<std::set<std::vector<int32_t>>>,
                      std::vector<std::set<std::vector<std::vector<int32_t>>>>>,
            std::vector<std::vector<int32_t>>>
            x35);
    int main();
    std::set<std::vector<std::vector<int32_t>>> x5;
    std::set<std::vector<int32_t>> x10;
    int main() {
      // begin Array.init
      std::vector<std::set<std::vector<int32_t>>> x7(100);
      std::vector<std::set<std::vector<int32_t>>> x8 = x7;
      for (int x9 = 0; x9 < 100; x9 += 1) {
        x8[x9] = x10;
      }
      // end Array.init
      std::vector<std::set<std::vector<int32_t>>> x11 = x8;
      // begin Array.init
      std::vector<std::set<std::vector<std::vector<int32_t>>>> x2(100);
      std::vector<std::set<std::vector<std::vector<int32_t>>>> x3 = x2;
      for (int x4 = 0; x4 < 100; x4 += 1) {
        x3[x4] = x5;
      }
      // end Array.init
      std::vector<std::set<std::vector<std::vector<int32_t>>>> x6 = x3;
      std::unique_ptr<sexp> x56 = sexp::load(std::cin);
      const std::vector<std::unique_ptr<sexp>> &x57 =
          ((list *)x56.get())->get_body();
      // begin Array.init
      std::vector<std::vector<int32_t>> x58((int)((x57).size()));
      std::vector<std::vector<int32_t>> x59 = x58;
      for (int x60 = 0; x60 < (int)((x57).size()); x60 += 1) {
        const std::vector<std::unique_ptr<sexp>> &x61 =
            ((list *)(x57)[x60].get())->get_body();
        // begin Array.init
        std::vector<int32_t> x62((int)((x61).size()));
        std::vector<int32_t> x63 = x62;
        for (int x64 = 0; x64 < (int)((x61).size()); x64 += 1) {
          int32_t x65 = std::stoi(((atom *)(x61)[x64].get())->get_body());
          x63[x64] = x65;
        }
        // end Array.init
        x59[x60] = x63;
      }
      // end Array.init
      (x6[1]).insert(x59);
      std::unique_ptr<sexp> x24 = sexp::load(std::cin);
      const std::vector<std::unique_ptr<sexp>> &x25 =
          ((list *)x24.get())->get_body();
      // begin Array.init
      std::vector<std::vector<int32_t>> x26((int)((x25).size()));
      std::vector<std::vector<int32_t>> x27 = x26;
      for (int x28 = 0; x28 < (int)((x25).size()); x28 += 1) {
        const std::vector<std::unique_ptr<sexp>> &x29 =
            ((list *)(x25)[x28].get())->get_body();
        // begin Array.init
        std::vector<int32_t> x30((int)((x29).size()));
        std::vector<int32_t> x31 = x30;
        for (int x32 = 0; x32 < (int)((x29).size()); x32 += 1) {
          int32_t x33 = std::stoi(((atom *)(x29)[x32].get())->get_body());
          x31[x32] = x33;
        }
        // end Array.init
        x27[x28] = x31;
      }
      // end Array.init
      std::vector<std::vector<int32_t>> x34 = x27;
      for (auto x12 = (x6[1]).begin(); x12 != (x6[1]).end(); ++x12) {
        std::vector<std::vector<int32_t>> x13 = *x12;
        // begin Array.init
        std::vector<std::vector<int32_t>> x14(((int)((x13).size())));
        std::vector<std::vector<int32_t>> x15 = x14;
        for (int x16 = 0; x16 < ((int)((x13).size())); x16 += 1) {
          std::vector<int32_t> x17 = (x13[x16]);
          int32_t x18 = ((int)((x17).size()));
          // begin Array.init
          std::vector<int32_t> x19(x18);
          std::vector<int32_t> x20 = x19;
          for (int x21 = 0; x21 < x18; x21 += 1) {
            int32_t x22 = (x17[((x18 - x21) - 1)]);
            x20[x21] = x22;
          }
          // end Array.init
          x15[x16] = x20;
        }
        // end Array.init
        std::vector<std::vector<int32_t>> x23 = x15;
        int x54;
        int x55 = x54;
        if ((x23 == x34)) {
          reconstruct_L_2(std::make_pair(std::make_pair(x11, x6), x34));
          exit(0);
          x55 = 0;
        } else {

          x55 = 0;
        }
        (x6[2]).insert(x23);
      }
      return 0;
    }
    int reconstruct_L_2(
        std::pair<
            std::pair<std::vector<std::set<std::vector<int32_t>>>,
                      std::vector<std::set<std::vector<std::vector<int32_t>>>>>,
            std::vector<std::vector<int32_t>>>
            x35) {
      std::pair<std::vector<std::set<std::vector<int32_t>>>,
                std::vector<std::set<std::vector<std::vector<int32_t>>>>>
          x36 = std::get<0>(x35);
      std::vector<std::vector<int32_t>> x37 = std::get<1>(x35);
      for (auto x38 = (std::get<1>(x36)[1]).begin();
           x38 != (std::get<1>(x36)[1]).end(); ++x38) {
        int x52;
        int x53 = x52;
        std::vector<std::vector<int32_t>> x39 = *x38;
        // begin Array.init
        std::vector<std::vector<int32_t>> x40(((int)((x39).size())));
        std::vector<std::vector<int32_t>> x41 = x40;
        for (int x42 = 0; x42 < ((int)((x39).size())); x42 += 1) {
          std::vector<int32_t> x43 = (x39[x42]);
          int32_t x44 = ((int)((x43).size()));
          // begin Array.init
          std::vector<int32_t> x45(x44);
          std::vector<int32_t> x46 = x45;
          for (int x47 = 0; x47 < x44; x47 += 1) {
            int32_t x48 = (x43[((x44 - x47) - 1)]);
            x46[x47] = x48;
          }
          // end Array.init
          x41[x42] = x46;
        }
        // end Array.init

        if ((0 || (x41 == x37))) {
          std::cout << "(App reverse ((App L0 ())))" << std::endl;
          reconstruct_L_1(std::make_pair(x36, *x38));
          x53 = 0;
        } else {

          x53 = 0;
        }
      }
      return 0;
    }
    int reconstruct_L_1(
        std::pair<
            std::pair<std::vector<std::set<std::vector<int32_t>>>,
                      std::vector<std::set<std::vector<std::vector<int32_t>>>>>,
            std::vector<std::vector<int32_t>>>
            x49) {
      std::pair<std::vector<std::set<std::vector<int32_t>>>,
                std::vector<std::set<std::vector<std::vector<int32_t>>>>>
          x50 = std::get<0>(x49);
      std::vector<std::vector<int32_t>> x51 = std::get<1>(x49);
      return 0;
    }
 |}];
  code |> Util.clang_build |> print_endline;
  [%expect {|
    <stdin>:105:68: warning: variable 'x54' is uninitialized when used here [-Wuninitialized]
      std::vector<std::vector<int32_t>> x23 = x15; int x54;  int x55 = x54;
                                                                       ^~~
    <stdin>:105:55: note: initialize the variable 'x54' to silence this warning
      std::vector<std::vector<int32_t>> x23 = x15; int x54;  int x55 = x54;
                                                          ^
                                                           = 0
    <stdin>:117:30: warning: variable 'x52' is uninitialized when used here [-Wuninitialized]
             int x52;  int x53 = x52;    std::vector<std::vector<int32_t>> x39 = *x38;
                                 ^~~
    <stdin>:117:17: note: initialize the variable 'x52' to silence this warning
             int x52;  int x53 = x52;    std::vector<std::vector<int32_t>> x39 = *x38;
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
  [%expect {|
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
            x52);
    int reconstruct_L_2(
        std::pair<std::pair<std::vector<std::set<std::vector<int32_t>>>,
                            std::vector<std::set<std::vector<span<int32_t>>>>>,
                  std::vector<span<int32_t>>>
            x38);
    int main();
    std::vector<int32_t> x2;
    std::vector<int32_t> x3;
    std::set<std::vector<span<int32_t>>> x7;
    std::set<std::vector<int32_t>> x12;
    std::vector<int32_t> x31;
    std::vector<int32_t> x64;
    int main() {
      // begin Array.init
      std::vector<std::set<std::vector<int32_t>>> x9(100);
      std::vector<std::set<std::vector<int32_t>>> x10 = x9;
      for (int x11 = 0; x11 < 100; x11 += 1) {
        x10[x11] = x12;
      }
      // end Array.init
      std::vector<std::set<std::vector<int32_t>>> x13 = x10;
      // begin Array.init
      std::vector<std::set<std::vector<span<int32_t>>>> x4(100);
      std::vector<std::set<std::vector<span<int32_t>>>> x5 = x4;
      for (int x6 = 0; x6 < 100; x6 += 1) {
        x5[x6] = x7;
      }
      // end Array.init
      std::vector<std::set<std::vector<span<int32_t>>>> x8 = x5;
      std::unique_ptr<sexp> x59 = sexp::load(std::cin);
      const std::vector<std::unique_ptr<sexp>> &x60 =
          ((list *)x59.get())->get_body();
      // begin Array.init
      std::vector<span<int32_t>> x61((int)((x60).size()));
      std::vector<span<int32_t>> x62 = x61;
      for (int x63 = 0; x63 < (int)((x60).size()); x63 += 1) {
        const std::vector<std::unique_ptr<sexp>> &x65 =
            ((list *)(x60)[x63].get())->get_body();
        // begin Array.init
        int32_t x66 = ((int)((x64).size()));
        (x64).resize((x64).size() + ((int)((x65).size())));
        span<int32_t> x67 = (span<int32_t>){x64.data() + x66, (int)((x65).size())};
        for (int x68 = 0; x68 < (int)((x65).size()); x68 += 1) {
          int32_t x69 = std::stoi(((atom *)(x65)[x68].get())->get_body());
          x67.ptr[x68] = x69;
        }
        // end Array.init
        x62[x63] = x67;
      }
      // end Array.init
      (x8[1]).insert(x62);
      std::unique_ptr<sexp> x26 = sexp::load(std::cin);
      const std::vector<std::unique_ptr<sexp>> &x27 =
          ((list *)x26.get())->get_body();
      // begin Array.init
      std::vector<span<int32_t>> x28((int)((x27).size()));
      std::vector<span<int32_t>> x29 = x28;
      for (int x30 = 0; x30 < (int)((x27).size()); x30 += 1) {
        const std::vector<std::unique_ptr<sexp>> &x32 =
            ((list *)(x27)[x30].get())->get_body();
        // begin Array.init
        int32_t x33 = ((int)((x31).size()));
        (x31).resize((x31).size() + ((int)((x32).size())));
        span<int32_t> x34 = (span<int32_t>){x31.data() + x33, (int)((x32).size())};
        for (int x35 = 0; x35 < (int)((x32).size()); x35 += 1) {
          int32_t x36 = std::stoi(((atom *)(x32)[x35].get())->get_body());
          x34.ptr[x35] = x36;
        }
        // end Array.init
        x29[x30] = x34;
      }
      // end Array.init
      std::vector<span<int32_t>> x37 = x29;
      for (auto x14 = (x8[1]).begin(); x14 != (x8[1]).end(); ++x14) {
        std::vector<span<int32_t>> x15 = *x14;
        // begin Array.init
        std::vector<span<int32_t>> x16(((int)((x15).size())));
        std::vector<span<int32_t>> x17 = x16;
        for (int x18 = 0; x18 < ((int)((x15).size())); x18 += 1) {
          span<int32_t> x19 = (x15[x18]);
          int32_t x20 = ((int)((x19).len));
          // begin Array.init
          int32_t x21 = ((int)((x3).size()));
          (x3).resize((x3).size() + (x20));
          span<int32_t> x22 = (span<int32_t>){x3.data() + x21, x20};
          for (int x23 = 0; x23 < x20; x23 += 1) {
            int32_t x24 = (x19.ptr[((x20 - x23) - 1)]);
            x22.ptr[x23] = x24;
          }
          // end Array.init
          x17[x18] = x22;
        }
        // end Array.init
        std::vector<span<int32_t>> x25 = x17;
        int x57;
        int x58 = x57;
        if ((x25 == x37)) {
          reconstruct_L_2(std::make_pair(std::make_pair(x13, x8), x37));
          exit(0);
          x58 = 0;
        } else {

          x58 = 0;
        }
        (x8[2]).insert(x25);
      }
      return 0;
    }
    int reconstruct_L_2(
        std::pair<std::pair<std::vector<std::set<std::vector<int32_t>>>,
                            std::vector<std::set<std::vector<span<int32_t>>>>>,
                  std::vector<span<int32_t>>>
            x38) {
      std::pair<std::vector<std::set<std::vector<int32_t>>>,
                std::vector<std::set<std::vector<span<int32_t>>>>>
          x39 = std::get<0>(x38);
      std::vector<span<int32_t>> x40 = std::get<1>(x38);
      for (auto x41 = (std::get<1>(x39)[1]).begin();
           x41 != (std::get<1>(x39)[1]).end(); ++x41) {
        int x55;
        int x56 = x55;
        std::vector<span<int32_t>> x42 = *x41;
        // begin Array.init
        std::vector<span<int32_t>> x43(((int)((x42).size())));
        std::vector<span<int32_t>> x44 = x43;
        for (int x45 = 0; x45 < ((int)((x42).size())); x45 += 1) {
          span<int32_t> x46 = (x42[x45]);
          int32_t x47 = ((int)((x46).len));
          // begin Array.init
          int32_t x48 = ((int)((x3).size()));
          (x3).resize((x3).size() + (x47));
          span<int32_t> x49 = (span<int32_t>){x3.data() + x48, x47};
          for (int x50 = 0; x50 < x47; x50 += 1) {
            int32_t x51 = (x46.ptr[((x47 - x50) - 1)]);
            x49.ptr[x50] = x51;
          }
          // end Array.init
          x44[x45] = x49;
        }
        // end Array.init

        if ((0 || (x44 == x40))) {
          std::cout << "(App reverse ((App L0 ())))" << std::endl;
          reconstruct_L_1(std::make_pair(x39, *x41));
          x56 = 0;
        } else {

          x56 = 0;
        }
      }
      return 0;
    }
    int reconstruct_L_1(
        std::pair<std::pair<std::vector<std::set<std::vector<int32_t>>>,
                            std::vector<std::set<std::vector<span<int32_t>>>>>,
                  std::vector<span<int32_t>>>
            x52) {
      std::pair<std::vector<std::set<std::vector<int32_t>>>,
                std::vector<std::set<std::vector<span<int32_t>>>>>
          x53 = std::get<0>(x52);
      std::vector<span<int32_t>> x54 = std::get<1>(x52);
      return 0;
    } |}];
  code |> Util.clang_build |> print_endline;
  [%expect {|
    <stdin>:105:61: warning: variable 'x57' is uninitialized when used here [-Wuninitialized]
      std::vector<span<int32_t>> x25 = x17; int x57;  int x58 = x57;
                                                                ^~~
    <stdin>:105:48: note: initialize the variable 'x57' to silence this warning
      std::vector<span<int32_t>> x25 = x17; int x57;  int x58 = x57;
                                                   ^
                                                    = 0
    <stdin>:117:30: warning: variable 'x55' is uninitialized when used here [-Wuninitialized]
             int x55;  int x56 = x55;    std::vector<span<int32_t>> x42 = *x41;
                                 ^~~
    <stdin>:117:17: note: initialize the variable 'x55' to silence this warning
             int x55;  int x56 = x55;    std::vector<span<int32_t>> x42 = *x41;
                    ^
                     = 0
    2 warnings generated. |}]

let%expect_test "" =
  let module Code = Mlstage.Code in
  let module Deepcoder = Deepcoder.Make (Code.Array) (Code) in
  let g = ("L", Grammar.Term.App ("input", [])) :: Deepcoder.Lang.grammar in
  let state = Random.State.make [||] in
  Grammar.sample_seq ~state "L" g
  |> Sequence.filter ~f:(fun t ->
         let n = Grammar.Term.size t in
         6 <= n && n <= 10)
  |> Sequence.filter_map ~f:(fun t ->
         try
           let input = Deepcoder.Value.random ~state "L" 5 in
           let input_str = input |> Deepcoder.Value.sexp_of |> Code.to_string in

           let output =
             Deepcoder.Lang.eval
               (Map.of_alist_exn (module String) [ ("input", input) ])
               t
             |> Deepcoder.Value.sexp_of |> Code.to_string
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
