open! Core

let%expect_test "" =
  let module Code = Cstage.Code () in
  let module Deepcoder = Deepcoder.Make (Code) in
  let module Sketch = struct
    let inputs = [ "L" ]

    let output = "L"
  end in
  let module DeepSynth =
    Synth.Make (Sketch) (Code) (Deepcoder.Lang) (Deepcoder.Cache)
  in
  DeepSynth.enumerate 2 |> Code.to_string |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>

    #include "sexp.hpp"
    int reconstruct_L_1(
        std::pair<std::pair<std::vector<std::set<
                                std::pair<std::vector<int>, std::vector<int>>>>,
                            std::vector<std::set<std::pair<
                                std::vector<std::vector<int>>, std::vector<int>>>>>,
                  std::vector<std::vector<int>>>
            x51);
    int reconstruct_L_2(
        std::pair<std::pair<std::vector<std::set<
                                std::pair<std::vector<int>, std::vector<int>>>>,
                            std::vector<std::set<std::pair<
                                std::vector<std::vector<int>>, std::vector<int>>>>>,
                  std::vector<std::vector<int>>>
            x37);
    int main();
    std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>> x5;
    std::set<std::pair<std::vector<int>, std::vector<int>>> x10;
    int main() {
      // begin Array.init
      std::vector<std::set<std::pair<std::vector<int>, std::vector<int>>>> x7(100);
      std::vector<std::set<std::pair<std::vector<int>, std::vector<int>>>> x8 = x7;
      for (int x9 = 0; x9 < 100; x9 += 1) {
        x8[x9] = x10;
      }
      // end Array.init
      std::vector<std::set<std::pair<std::vector<int>, std::vector<int>>>> x11 = x8;
      // begin Array.init
      std::vector<
          std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>>>
          x2(100);
      std::vector<
          std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>>>
          x3 = x2;
      for (int x4 = 0; x4 < 100; x4 += 1) {
        x3[x4] = x5;
      }
      // end Array.init
      std::vector<
          std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>>>
          x6 = x3;
      std::unique_ptr<sexp> x58 = sexp::load(std::cin);
      const std::vector<std::unique_ptr<sexp>> &x59 =
          ((list *)x58.get())->get_body();
      // begin Array.init
      std::vector<std::vector<int>> x60((int)((x59).size()));
      std::vector<std::vector<int>> x61 = x60;
      for (int x62 = 0; x62 < (int)((x59).size()); x62 += 1) {
        const std::vector<std::unique_ptr<sexp>> &x63 =
            ((list *)(x59)[x62].get())->get_body();
        // begin Array.init
        std::vector<int> x64((int)((x63).size()));
        std::vector<int> x65 = x64;
        for (int x66 = 0; x66 < (int)((x63).size()); x66 += 1) {
          int x67 = std::stoi(((atom *)(x63)[x66].get())->get_body());
          x65[x66] = x67;
        }
        // end Array.init
        x61[x62] = x65;
      }
      // end Array.init

      // begin Array.const
      std::vector<int> x68(0);
      std::vector<int> x69 = x68; // end Array.const
      (x6[1]).insert(std::make_pair(x61, x69));
      std::unique_ptr<sexp> x26 = sexp::load(std::cin);
      const std::vector<std::unique_ptr<sexp>> &x27 =
          ((list *)x26.get())->get_body();
      // begin Array.init
      std::vector<std::vector<int>> x28((int)((x27).size()));
      std::vector<std::vector<int>> x29 = x28;
      for (int x30 = 0; x30 < (int)((x27).size()); x30 += 1) {
        const std::vector<std::unique_ptr<sexp>> &x31 =
            ((list *)(x27)[x30].get())->get_body();
        // begin Array.init
        std::vector<int> x32((int)((x31).size()));
        std::vector<int> x33 = x32;
        for (int x34 = 0; x34 < (int)((x31).size()); x34 += 1) {
          int x35 = std::stoi(((atom *)(x31)[x34].get())->get_body());
          x33[x34] = x35;
        }
        // end Array.init
        x29[x30] = x33;
      }
      // end Array.init
      std::vector<std::vector<int>> x36 = x29;
      for (auto x12 = (x6[1]).begin(); x12 != (x6[1]).end(); ++x12) {
        std::vector<std::vector<int>> x13 = std::get<0>(*x12);
        // begin Array.init
        std::vector<std::vector<int>> x14(((int)((x13).size())));
        std::vector<std::vector<int>> x15 = x14;
        for (int x16 = 0; x16 < ((int)((x13).size())); x16 += 1) {
          std::vector<int> x17 = (x13[x16]);
          int x18 = ((int)((x17).size()));
          // begin Array.init
          std::vector<int> x19(x18);
          std::vector<int> x20 = x19;
          for (int x21 = 0; x21 < x18; x21 += 1) {
            int x22 = (x17[((x18 - x21) - 1)]);
            x20[x21] = x22;
          }
          // end Array.init
          x15[x16] = x20;
        }
        // end Array.init
        std::vector<std::vector<int>> x23 = x15;
        int x56;
        int x57 = x56;
        if ((x23 == x36)) {
          reconstruct_L_2(std::make_pair(std::make_pair(x11, x6), x36));
          exit(0);
          x57 = 0;
        } else {

          x57 = 0;
        }

        // begin Array.const
        std::vector<int> x24(0);
        std::vector<int> x25 = x24; // end Array.const
        (x6[2]).insert(std::make_pair(x23, x25));
      }
      return 0;
    }
    int reconstruct_L_2(
        std::pair<std::pair<std::vector<std::set<
                                std::pair<std::vector<int>, std::vector<int>>>>,
                            std::vector<std::set<std::pair<
                                std::vector<std::vector<int>>, std::vector<int>>>>>,
                  std::vector<std::vector<int>>>
            x37) {
      std::pair<
          std::vector<std::set<std::pair<std::vector<int>, std::vector<int>>>>,
          std::vector<
              std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>>>>
          x38 = std::get<0>(x37);
      std::vector<std::vector<int>> x39 = std::get<1>(x37);
      for (auto x40 = (std::get<1>(x38)[1]).begin();
           x40 != (std::get<1>(x38)[1]).end(); ++x40) {
        int x54;
        int x55 = x54;
        std::vector<std::vector<int>> x41 = std::get<0>(*x40);
        // begin Array.init
        std::vector<std::vector<int>> x42(((int)((x41).size())));
        std::vector<std::vector<int>> x43 = x42;
        for (int x44 = 0; x44 < ((int)((x41).size())); x44 += 1) {
          std::vector<int> x45 = (x41[x44]);
          int x46 = ((int)((x45).size()));
          // begin Array.init
          std::vector<int> x47(x46);
          std::vector<int> x48 = x47;
          for (int x49 = 0; x49 < x46; x49 += 1) {
            int x50 = (x45[((x46 - x49) - 1)]);
            x48[x49] = x50;
          }
          // end Array.init
          x43[x44] = x48;
        }
        // end Array.init

        if ((0 || (x43 == x39))) {
          std::cout << "(App reverse ((App L0 ())))" << std::endl;
          reconstruct_L_1(std::make_pair(x38, std::get<0>(*x40)));
          x55 = 0;
        } else {

          x55 = 0;
        }
      }
      return 0;
    }
    int reconstruct_L_1(
        std::pair<std::pair<std::vector<std::set<
                                std::pair<std::vector<int>, std::vector<int>>>>,
                            std::vector<std::set<std::pair<
                                std::vector<std::vector<int>>, std::vector<int>>>>>,
                  std::vector<std::vector<int>>>
            x51) {
      std::pair<
          std::vector<std::set<std::pair<std::vector<int>, std::vector<int>>>>,
          std::vector<
              std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>>>>
          x52 = std::get<0>(x51);
      std::vector<std::vector<int>> x53 = std::get<1>(x51);
      return 0;
    }
 |}]

let%expect_test "" =
  let module Code = Mlstage.Code in
  let module Deepcoder = Deepcoder.Make (Code) in
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
  [%expect {|
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
