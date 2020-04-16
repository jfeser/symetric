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
    std::vector<sexp *> x25;
    std::vector<std::vector<int32_t>> x34;
    std::vector<std::vector<int32_t>> x62;
    int main() {
      std::set<std::vector<std::vector<int32_t>>> x5;
      std::set<std::vector<int32_t>> x10;
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
      sexp *x24 = sexp::load(std::cin);
      x25 = ((list *)x24)->get_body();
      std::vector<sexp *> x26 = ((list *)(x25)[1])->get_body();
      // begin Array.init
      std::vector<std::vector<int32_t>> x27((int)((x26).size()));
      std::vector<std::vector<int32_t>> x28 = x27;
      for (int x29 = 0; x29 < (int)((x26).size()); x29 += 1) {
        std::vector<sexp *> x30 = ((list *)(x26)[x29])->get_body();
        // begin Array.init
        std::vector<int32_t> x31((int)((x30).size()));
        std::vector<int32_t> x32 = x31;
        for (int x33 = 0; x33 < (int)((x30).size()); x33 += 1) {
          x32[x33] = std::stoi(((atom *)(x30)[x33])->get_body());
        }
        // end Array.init
        x28[x29] = x32;
      }
      // end Array.init
      x34 = x28;
      std::vector<sexp *> x54 = ((list *)(x25)[0])->get_body();
      // begin Array.init
      std::vector<std::vector<int32_t>> x55((int)((x54).size()));
      std::vector<std::vector<int32_t>> x56 = x55;
      for (int x57 = 0; x57 < (int)((x54).size()); x57 += 1) {
        std::vector<sexp *> x58 = ((list *)(x54)[x57])->get_body();
        // begin Array.init
        std::vector<int32_t> x59((int)((x58).size()));
        std::vector<int32_t> x60 = x59;
        for (int x61 = 0; x61 < (int)((x58).size()); x61 += 1) {
          x60[x61] = std::stoi(((atom *)(x58)[x61])->get_body());
        }
        // end Array.init
        x56[x57] = x60;
      }
      // end Array.init
      x62 = x56;
      int x90;
      int x91 = x90;
      if ((1 == 1)) {
        std::vector<std::vector<int32_t>> x78 = x62;
        // begin Array.init
        std::vector<std::vector<int32_t>> x79(((int)((x78).size())));
        std::vector<std::vector<int32_t>> x80 = x79;
        for (int x81 = 0; x81 < ((int)((x78).size())); x81 += 1) {
          std::vector<int32_t> x82 = (x78[x81]);
          int32_t x83 = ((int)((x82).size()));
          // begin Array.init
          std::vector<int32_t> x84(x83);
          std::vector<int32_t> x85 = x84;
          for (int x86 = 0; x86 < x83; x86 += 1) {
            x85[x86] = (x82[((x83 - x86) - 1)]);
          }
          // end Array.init
          x80[x81] = x85;
        }
        // end Array.init
        std::vector<std::vector<int32_t>> x87 = x80;
        int x88;
        int x89 = x88;
        if ((x87 == x34)) {
          reconstruct_L_2(std::make_pair(std::make_pair(x11, x6), x34));
          exit(0);
          x89 = 0;
        } else {

          x89 = 0;
        }
        (x6[2]).insert(x87);
        x91 = 0;
      } else {

        x91 = 0;
      }

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
        int x76;
        int x77 = x76;
        if ((x23 == x34)) {
          reconstruct_L_2(std::make_pair(std::make_pair(x11, x6), x34));
          exit(0);
          x77 = 0;
        } else {

          x77 = 0;
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
      int x74;
      int x75 = x74;
      if ((1 == 1)) {
        int x72;
        int x73 = x72;
        std::vector<std::vector<int32_t>> x63 = x62;
        // begin Array.init
        std::vector<std::vector<int32_t>> x64(((int)((x63).size())));
        std::vector<std::vector<int32_t>> x65 = x64;
        for (int x66 = 0; x66 < ((int)((x63).size())); x66 += 1) {
          std::vector<int32_t> x67 = (x63[x66]);
          int32_t x68 = ((int)((x67).size()));
          // begin Array.init
          std::vector<int32_t> x69(x68);
          std::vector<int32_t> x70 = x69;
          for (int x71 = 0; x71 < x68; x71 += 1) {
            x70[x71] = (x67[((x68 - x71) - 1)]);
          }
          // end Array.init
          x65[x66] = x70;
        }
        // end Array.init

        if ((0 || (x65 == x37))) {
          std::cout << "(App reverse ((App L0 ())))" << std::endl;
          reconstruct_L_1(std::make_pair(x36, x62));
          x73 = 0;
        } else {

          x73 = 0;
        }

        x75 = x73;
      } else {

        x75 = 0;
      }

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
  [%expect
    {|
    <stdin>:134:68: warning: variable 'x76' is uninitialized when used here [-Wuninitialized]
      std::vector<std::vector<int32_t>> x23 = x15; int x76;  int x77 = x76;
                                                                       ^~~
    <stdin>:134:55: note: initialize the variable 'x76' to silence this warning
      std::vector<std::vector<int32_t>> x23 = x15; int x76;  int x77 = x76;
                                                          ^
                                                           = 0
    <stdin>:106:68: warning: variable 'x88' is uninitialized when used here [-Wuninitialized]
      std::vector<std::vector<int32_t>> x87 = x80; int x88;  int x89 = x88;
                                                                       ^~~
    <stdin>:106:55: note: initialize the variable 'x88' to silence this warning
      std::vector<std::vector<int32_t>> x87 = x80; int x88;  int x89 = x88;
                                                          ^
                                                           = 0
    <stdin>:90:33: warning: variable 'x90' is uninitialized when used here [-Wuninitialized]
     x62 = x56; int x90;  int x91 = x90;
                                    ^~~
    <stdin>:90:20: note: initialize the variable 'x90' to silence this warning
     x62 = x56; int x90;  int x91 = x90;
                       ^
                        = 0
    <stdin>:174:30: warning: variable 'x52' is uninitialized when used here [-Wuninitialized]
             int x52;  int x53 = x52;    std::vector<std::vector<int32_t>> x39 = *x38;
                                 ^~~
    <stdin>:174:17: note: initialize the variable 'x52' to silence this warning
             int x52;  int x53 = x52;    std::vector<std::vector<int32_t>> x39 = *x38;
                    ^
                     = 0
    <stdin>:146:24: warning: variable 'x72' is uninitialized when used here [-Wuninitialized]
       int x72;  int x73 = x72;    std::vector<std::vector<int32_t>> x63 = x62;
                           ^~~
    <stdin>:146:11: note: initialize the variable 'x72' to silence this warning
       int x72;  int x73 = x72;    std::vector<std::vector<int32_t>> x63 = x62;
              ^
               = 0
    <stdin>:144:410: warning: variable 'x74' is uninitialized when used here [-Wuninitialized]
    int reconstruct_L_2(std::pair<std::pair<std::vector<std::set<std::vector<int32_t> >>,std::vector<std::set<std::vector<std::vector<int32_t>> >> >,std::vector<std::vector<int32_t>> > x35) {    std::pair<std::vector<std::set<std::vector<int32_t> >>,std::vector<std::set<std::vector<std::vector<int32_t>> >> > x36 = std::get<0>(x35);  std::vector<std::vector<int32_t>> x37 = std::get<1>(x35); int x74;  int x75 = x74;
                                                                                                                                                                                                                                                                                                                                                                                                                             ^~~
    <stdin>:144:397: note: initialize the variable 'x74' to silence this warning
    int reconstruct_L_2(std::pair<std::pair<std::vector<std::set<std::vector<int32_t> >>,std::vector<std::set<std::vector<std::vector<int32_t>> >> >,std::vector<std::vector<int32_t>> > x35) {    std::pair<std::vector<std::set<std::vector<int32_t> >>,std::vector<std::set<std::vector<std::vector<int32_t>> >> > x36 = std::get<0>(x35);  std::vector<std::vector<int32_t>> x37 = std::get<1>(x35); int x74;  int x75 = x74;
                                                                                                                                                                                                                                                                                                                                                                                                                ^
                                                                                                                                                                                                                                                                                                                                                                                                                 = 0
    6 warnings generated. |}]

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
            x55);
    int reconstruct_L_2(
        std::pair<std::pair<std::vector<std::set<std::vector<int32_t>>>,
                            std::vector<std::set<std::vector<span<int32_t>>>>>,
                  std::vector<span<int32_t>>>
            x41);
    int main();
    std::array<int32_t, 500000> x2;
    int32_t x3;
    std::array<int32_t, 500000> x4;
    int32_t x5;
    std::vector<sexp *> x29;
    std::array<int32_t, 500000> x34;
    int32_t x35;
    std::vector<span<int32_t>> x40;
    std::array<int32_t, 500000> x64;
    int32_t x65;
    std::vector<span<int32_t>> x70;
    int main() {
      std::set<std::vector<span<int32_t>>> x9;
      std::set<std::vector<int32_t>> x14;
      // begin Array.init
      std::vector<std::set<std::vector<int32_t>>> x11(100);
      std::vector<std::set<std::vector<int32_t>>> x12 = x11;
      for (int x13 = 0; x13 < 100; x13 += 1) {
        x12[x13] = x14;
      }
      // end Array.init
      std::vector<std::set<std::vector<int32_t>>> x15 = x12;
      // begin Array.init
      std::vector<std::set<std::vector<span<int32_t>>>> x6(100);
      std::vector<std::set<std::vector<span<int32_t>>>> x7 = x6;
      for (int x8 = 0; x8 < 100; x8 += 1) {
        x7[x8] = x9;
      }
      // end Array.init
      std::vector<std::set<std::vector<span<int32_t>>>> x10 = x7;
      sexp *x28 = sexp::load(std::cin);
      x29 = ((list *)x28)->get_body();
      std::vector<sexp *> x30 = ((list *)(x29)[1])->get_body();
      // begin Array.init
      std::vector<span<int32_t>> x31((int)((x30).size()));
      std::vector<span<int32_t>> x32 = x31;
      for (int x33 = 0; x33 < (int)((x30).size()); x33 += 1) {
        std::vector<sexp *> x36 = ((list *)(x30)[x33])->get_body();
        // begin Array.init
        span<int32_t> x37 =
            (span<int32_t>){(x34).data() + x35, (int)((x36).size())};
        x35 += (int)((x36).size());
        span<int32_t> x38 = x37;
        for (int x39 = 0; x39 < (int)((x36).size()); x39 += 1) {
          x38.ptr[x39] = std::stoi(((atom *)(x36)[x39])->get_body());
        }
        // end Array.init
        x32[x33] = x38;
      }
      // end Array.init
      x40 = x32;
      std::vector<sexp *> x60 = ((list *)(x29)[0])->get_body();
      // begin Array.init
      std::vector<span<int32_t>> x61((int)((x60).size()));
      std::vector<span<int32_t>> x62 = x61;
      for (int x63 = 0; x63 < (int)((x60).size()); x63 += 1) {
        std::vector<sexp *> x66 = ((list *)(x60)[x63])->get_body();
        // begin Array.init
        span<int32_t> x67 =
            (span<int32_t>){(x64).data() + x65, (int)((x66).size())};
        x65 += (int)((x66).size());
        span<int32_t> x68 = x67;
        for (int x69 = 0; x69 < (int)((x66).size()); x69 += 1) {
          x68.ptr[x69] = std::stoi(((atom *)(x66)[x69])->get_body());
        }
        // end Array.init
        x62[x63] = x68;
      }
      // end Array.init
      x70 = x62;
      int x98;
      int x99 = x98;
      if ((1 == 1)) {
        std::vector<span<int32_t>> x86 = x70;
        // begin Array.init
        std::vector<span<int32_t>> x87(((int)((x86).size())));
        std::vector<span<int32_t>> x88 = x87;
        for (int x89 = 0; x89 < ((int)((x86).size())); x89 += 1) {
          span<int32_t> x90 = (x86[x89]);
          int32_t x91 = ((int)((x90).len));
          // begin Array.init
          span<int32_t> x92 = (span<int32_t>){(x4).data() + x5, x91};
          x5 += x91;
          span<int32_t> x93 = x92;
          for (int x94 = 0; x94 < x91; x94 += 1) {
            x93.ptr[x94] = (x90.ptr[((x91 - x94) - 1)]);
          }
          // end Array.init
          x88[x89] = x93;
        }
        // end Array.init
        std::vector<span<int32_t>> x95 = x88;
        int x96;
        int x97 = x96;
        if ((x95 == x40)) {
          reconstruct_L_2(std::make_pair(std::make_pair(x15, x10), x40));
          exit(0);
          x97 = 0;
        } else {

          x97 = 0;
        }
        (x10[2]).insert(x95);
        x99 = 0;
      } else {

        x99 = 0;
      }

      for (auto x16 = (x10[1]).begin(); x16 != (x10[1]).end(); ++x16) {
        std::vector<span<int32_t>> x17 = *x16;
        // begin Array.init
        std::vector<span<int32_t>> x18(((int)((x17).size())));
        std::vector<span<int32_t>> x19 = x18;
        for (int x20 = 0; x20 < ((int)((x17).size())); x20 += 1) {
          span<int32_t> x21 = (x17[x20]);
          int32_t x22 = ((int)((x21).len));
          // begin Array.init
          span<int32_t> x23 = (span<int32_t>){(x4).data() + x5, x22};
          x5 += x22;
          span<int32_t> x24 = x23;
          for (int x25 = 0; x25 < x22; x25 += 1) {
            int32_t x26 = (x21.ptr[((x22 - x25) - 1)]);
            x24.ptr[x25] = x26;
          }
          // end Array.init
          x19[x20] = x24;
        }
        // end Array.init
        std::vector<span<int32_t>> x27 = x19;
        int x84;
        int x85 = x84;
        if ((x27 == x40)) {
          reconstruct_L_2(std::make_pair(std::make_pair(x15, x10), x40));
          exit(0);
          x85 = 0;
        } else {

          x85 = 0;
        }
        (x10[2]).insert(x27);
      }
      return 0;
    }
    int reconstruct_L_2(
        std::pair<std::pair<std::vector<std::set<std::vector<int32_t>>>,
                            std::vector<std::set<std::vector<span<int32_t>>>>>,
                  std::vector<span<int32_t>>>
            x41) {
      std::pair<std::vector<std::set<std::vector<int32_t>>>,
                std::vector<std::set<std::vector<span<int32_t>>>>>
          x42 = std::get<0>(x41);
      std::vector<span<int32_t>> x43 = std::get<1>(x41);
      int x82;
      int x83 = x82;
      if ((1 == 1)) {
        int x80;
        int x81 = x80;
        std::vector<span<int32_t>> x71 = x70;
        // begin Array.init
        std::vector<span<int32_t>> x72(((int)((x71).size())));
        std::vector<span<int32_t>> x73 = x72;
        for (int x74 = 0; x74 < ((int)((x71).size())); x74 += 1) {
          span<int32_t> x75 = (x71[x74]);
          int32_t x76 = ((int)((x75).len));
          // begin Array.init
          span<int32_t> x77 = (span<int32_t>){(x4).data() + x5, x76};
          x5 += x76;
          span<int32_t> x78 = x77;
          for (int x79 = 0; x79 < x76; x79 += 1) {
            x78.ptr[x79] = (x75.ptr[((x76 - x79) - 1)]);
          }
          // end Array.init
          x73[x74] = x78;
        }
        // end Array.init

        if ((0 || (x73 == x43))) {
          std::cout << "(App reverse ((App L0 ())))" << std::endl;
          reconstruct_L_1(std::make_pair(x42, x70));
          x81 = 0;
        } else {

          x81 = 0;
        }

        x83 = x81;
      } else {

        x83 = 0;
      }

      for (auto x44 = (std::get<1>(x42)[1]).begin();
           x44 != (std::get<1>(x42)[1]).end(); ++x44) {
        int x58;
        int x59 = x58;
        std::vector<span<int32_t>> x45 = *x44;
        // begin Array.init
        std::vector<span<int32_t>> x46(((int)((x45).size())));
        std::vector<span<int32_t>> x47 = x46;
        for (int x48 = 0; x48 < ((int)((x45).size())); x48 += 1) {
          span<int32_t> x49 = (x45[x48]);
          int32_t x50 = ((int)((x49).len));
          // begin Array.init
          span<int32_t> x51 = (span<int32_t>){(x4).data() + x5, x50};
          x5 += x50;
          span<int32_t> x52 = x51;
          for (int x53 = 0; x53 < x50; x53 += 1) {
            int32_t x54 = (x49.ptr[((x50 - x53) - 1)]);
            x52.ptr[x53] = x54;
          }
          // end Array.init
          x47[x48] = x52;
        }
        // end Array.init

        if ((0 || (x47 == x43))) {
          std::cout << "(App reverse ((App L0 ())))" << std::endl;
          reconstruct_L_1(std::make_pair(x42, *x44));
          x59 = 0;
        } else {

          x59 = 0;
        }
      }
      return 0;
    }
    int reconstruct_L_1(
        std::pair<std::pair<std::vector<std::set<std::vector<int32_t>>>,
                            std::vector<std::set<std::vector<span<int32_t>>>>>,
                  std::vector<span<int32_t>>>
            x55) {
      std::pair<std::vector<std::set<std::vector<int32_t>>>,
                std::vector<std::set<std::vector<span<int32_t>>>>>
          x56 = std::get<0>(x55);
      std::vector<span<int32_t>> x57 = std::get<1>(x55);
      return 0;
    } |}];
  code |> Util.clang_build |> print_endline;
  [%expect
    {|
    <stdin>:134:61: warning: variable 'x84' is uninitialized when used here [-Wuninitialized]
      std::vector<span<int32_t>> x27 = x19; int x84;  int x85 = x84;
                                                                ^~~
    <stdin>:134:48: note: initialize the variable 'x84' to silence this warning
      std::vector<span<int32_t>> x27 = x19; int x84;  int x85 = x84;
                                                   ^
                                                    = 0
    <stdin>:106:61: warning: variable 'x96' is uninitialized when used here [-Wuninitialized]
      std::vector<span<int32_t>> x95 = x88; int x96;  int x97 = x96;
                                                                ^~~
    <stdin>:106:48: note: initialize the variable 'x96' to silence this warning
      std::vector<span<int32_t>> x95 = x88; int x96;  int x97 = x96;
                                                   ^
                                                    = 0
    <stdin>:90:33: warning: variable 'x98' is uninitialized when used here [-Wuninitialized]
     x70 = x62; int x98;  int x99 = x98;
                                    ^~~
    <stdin>:90:20: note: initialize the variable 'x98' to silence this warning
     x70 = x62; int x98;  int x99 = x98;
                       ^
                        = 0
    <stdin>:174:30: warning: variable 'x58' is uninitialized when used here [-Wuninitialized]
             int x58;  int x59 = x58;    std::vector<span<int32_t>> x45 = *x44;
                                 ^~~
    <stdin>:174:17: note: initialize the variable 'x58' to silence this warning
             int x58;  int x59 = x58;    std::vector<span<int32_t>> x45 = *x44;
                    ^
                     = 0
    <stdin>:146:24: warning: variable 'x80' is uninitialized when used here [-Wuninitialized]
       int x80;  int x81 = x80;    std::vector<span<int32_t>> x71 = x70;
                           ^~~
    <stdin>:146:11: note: initialize the variable 'x80' to silence this warning
       int x80;  int x81 = x80;    std::vector<span<int32_t>> x71 = x70;
              ^
               = 0
    <stdin>:144:382: warning: variable 'x82' is uninitialized when used here [-Wuninitialized]
    int reconstruct_L_2(std::pair<std::pair<std::vector<std::set<std::vector<int32_t> >>,std::vector<std::set<std::vector<span<int32_t>> >> >,std::vector<span<int32_t>> > x41) {    std::pair<std::vector<std::set<std::vector<int32_t> >>,std::vector<std::set<std::vector<span<int32_t>> >> > x42 = std::get<0>(x41);  std::vector<span<int32_t>> x43 = std::get<1>(x41); int x82;  int x83 = x82;
                                                                                                                                                                                                                                                                                                                                                                                                 ^~~
    <stdin>:144:369: note: initialize the variable 'x82' to silence this warning
    int reconstruct_L_2(std::pair<std::pair<std::vector<std::set<std::vector<int32_t> >>,std::vector<std::set<std::vector<span<int32_t>> >> >,std::vector<span<int32_t>> > x41) {    std::pair<std::vector<std::set<std::vector<int32_t> >>,std::vector<std::set<std::vector<span<int32_t>> >> > x42 = std::get<0>(x41);  std::vector<span<int32_t>> x43 = std::get<1>(x41); int x82;  int x83 = x82;
                                                                                                                                                                                                                                                                                                                                                                                    ^
                                                                                                                                                                                                                                                                                                                                                                                     = 0
    6 warnings generated. |}]

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
