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
    int reconstruct_L_1(const std::vector<std::vector<int>> &x40);
    int reconstruct_L_2(const std::vector<std::vector<int>> &x29);
    int main();
    std::vector<std::set<std::pair<std::vector<int>, std::vector<int>>>> x2;
    std::set<std::pair<std::vector<int>, std::vector<int>>> x4;
    std::vector<std::set<std::pair<std::vector<int>, std::vector<int>>>> x5;
    std::vector<
        std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>>>
        x6;
    std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>> x8;
    std::vector<
        std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>>>
        x9;
    std::vector<std::vector<int>> x12;
    std::vector<int> x16;
    std::vector<int> x20(0);
    std::vector<std::vector<int>> x22;
    std::vector<int> x25;
    int x43;
    std::vector<std::vector<int>> x45;
    std::vector<int> x48;
    std::vector<int> x51(0);
    int main() {
      // begin Array.init
      x2.clear();
      x2.reserve(100);
      for (int x3 = 0; x3 < 100; x3 += 1) {
        x2.push_back(x4);
      }
      // end Array.init
      x5 = x2;
      // begin Array.init
      x6.clear();
      x6.reserve(100);
      for (int x7 = 0; x7 < 100; x7 += 1) {
        x6.push_back(x8);
      }
      // end Array.init
      x9 = x6;
      const std::vector<std::unique_ptr<sexp>> &x44 =
          ((list *)sexp::load(std::cin).get())->get_body();
      // begin Array.init
      x45.clear();
      x45.reserve((int)((x44).size()));
      for (int x46 = 0; x46 < (int)((x44).size()); x46 += 1) {
        const std::vector<std::unique_ptr<sexp>> &x47 =
            ((list *)(x44)[x46].get())->get_body();
        // begin Array.init
        x48.clear();
        x48.reserve((int)((x47).size()));
        for (int x49 = 0; x49 < (int)((x47).size()); x49 += 1) {
          int x50 = std::stoi(((atom *)(x47)[x49].get())->get_body());
          x48.push_back(x50);
        }
        // end Array.init
        x45.push_back(x48);
      }
      // end Array.init

      // begin Array.const
      // end Array.const
      (x9[1]).insert(std::make_pair(x45, x51));
      const std::vector<std::unique_ptr<sexp>> &x21 =
          ((list *)sexp::load(std::cin).get())->get_body();
      // begin Array.init
      x22.clear();
      x22.reserve((int)((x21).size()));
      for (int x23 = 0; x23 < (int)((x21).size()); x23 += 1) {
        const std::vector<std::unique_ptr<sexp>> &x24 =
            ((list *)(x21)[x23].get())->get_body();
        // begin Array.init
        x25.clear();
        x25.reserve((int)((x24).size()));
        for (int x26 = 0; x26 < (int)((x24).size()); x26 += 1) {
          int x27 = std::stoi(((atom *)(x24)[x26].get())->get_body());
          x25.push_back(x27);
        }
        // end Array.init
        x22.push_back(x25);
      }
      // end Array.init
      std::vector<std::vector<int>> x28 = x22;
      for (auto x10 = (x9[2]).begin(); x10 != (x9[2]).end(); ++x10) {
        std::vector<std::vector<int>> x11 = std::get<0>(*x10);
        // begin Array.init
        x12.clear();
        x12.reserve(((int)((x11).size())));
        for (int x13 = 0; x13 < ((int)((x11).size())); x13 += 1) {
          std::vector<int> x14 = (x11[x13]);
          int x15 = ((int)((x14).size()));
          // begin Array.init
          x16.clear();
          x16.reserve(x15);
          for (int x17 = 0; x17 < x15; x17 += 1) {
            int x18 = (x14[((x15 - x17) - 1)]);
            x16.push_back(x18);
          }
          // end Array.init
          x12.push_back(x16);
        }
        // end Array.init
        std::vector<std::vector<int>> x19 = x12;
        if ((x19 == x28)) {
          std::cout << "Starting reconstruction" << std::endl;
          reconstruct_L_2(x28);
          exit(0);
          x43 = 0;
        } else {

          x43 = 0;
        }
        std::cout << "Inserting (L -> reverse(L)) cost 2" << std::endl;
        // begin Array.const
        // end Array.const
        (x9[2]).insert(std::make_pair(x19, x20));
      }
      return 0;
    }
    int reconstruct_L_2(const std::vector<std::vector<int>> &x29) {
      std::vector<std::vector<int>> x33;
      std::vector<int> x37;
      int x42;
      std::vector<std::vector<int>> x30 = x29;
      for (auto x31 = (x9[1]).begin(); x31 != (x9[1]).end(); ++x31) {
        std::vector<std::vector<int>> x32 = std::get<0>(*x31);
        // begin Array.init
        x33.clear();
        x33.reserve(((int)((x32).size())));
        for (int x34 = 0; x34 < ((int)((x32).size())); x34 += 1) {
          std::vector<int> x35 = (x32[x34]);
          int x36 = ((int)((x35).size()));
          // begin Array.init
          x37.clear();
          x37.reserve(x36);
          for (int x38 = 0; x38 < x36; x38 += 1) {
            int x39 = (x35[((x36 - x38) - 1)]);
            x37.push_back(x39);
          }
          // end Array.init
          x33.push_back(x37);
        }
        // end Array.init

        if ((0 || (x33 == x30))) {
          std::cout << "(App reverse ((App L0 ())))" << std::endl;
          reconstruct_L_1(std::get<0>(*x31));
          x42 = 0;
        } else {

          x42 = 0;
        }
      }
      return 0;
    }
    int reconstruct_L_1(const std::vector<std::vector<int>> &x40) {
      std::vector<std::vector<int>> x41 = x40;
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
