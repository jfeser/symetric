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
    int reconstruct_L_1(const std::vector<std::vector<int>> &x41);
    int reconstruct_L_2(const std::vector<std::vector<int>> &x30);
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
    std::vector<std::vector<int>> x23;
    std::vector<int> x26;
    int x44;
    std::vector<std::vector<int>> x47;
    std::vector<int> x50;
    std::vector<int> x53(0);
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
      std::unique_ptr<sexp> x45 = sexp::load(std::cin);
      const std::vector<std::unique_ptr<sexp>> &x46 =
          ((list *)x45.get())->get_body();
      // begin Array.init
      x47.clear();
      x47.reserve((int)((x46).size()));
      for (int x48 = 0; x48 < (int)((x46).size()); x48 += 1) {
        const std::vector<std::unique_ptr<sexp>> &x49 =
            ((list *)(x46)[x48].get())->get_body();
        // begin Array.init
        x50.clear();
        x50.reserve((int)((x49).size()));
        for (int x51 = 0; x51 < (int)((x49).size()); x51 += 1) {
          int x52 = std::stoi(((atom *)(x49)[x51].get())->get_body());
          x50.push_back(x52);
        }
        // end Array.init
        x47.push_back(x50);
      }
      // end Array.init

      // begin Array.const
      // end Array.const
      (x9[1]).insert(std::make_pair(x47, x53));
      std::unique_ptr<sexp> x21 = sexp::load(std::cin);
      const std::vector<std::unique_ptr<sexp>> &x22 =
          ((list *)x21.get())->get_body();
      // begin Array.init
      x23.clear();
      x23.reserve((int)((x22).size()));
      for (int x24 = 0; x24 < (int)((x22).size()); x24 += 1) {
        const std::vector<std::unique_ptr<sexp>> &x25 =
            ((list *)(x22)[x24].get())->get_body();
        // begin Array.init
        x26.clear();
        x26.reserve((int)((x25).size()));
        for (int x27 = 0; x27 < (int)((x25).size()); x27 += 1) {
          int x28 = std::stoi(((atom *)(x25)[x27].get())->get_body());
          x26.push_back(x28);
        }
        // end Array.init
        x23.push_back(x26);
      }
      // end Array.init
      std::vector<std::vector<int>> x29 = x23;
      for (auto x10 = (x9[1]).begin(); x10 != (x9[1]).end(); ++x10) {
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
        if ((x19 == x29)) {
          reconstruct_L_2(x29);
          exit(0);
          x44 = 0;
        } else {

          x44 = 0;
        }

        // begin Array.const
        // end Array.const
        (x9[2]).insert(std::make_pair(x19, x20));
      }
      return 0;
    }
    int reconstruct_L_2(const std::vector<std::vector<int>> &x30) {
      std::vector<std::vector<int>> x34;
      std::vector<int> x38;
      int x43;
      std::vector<std::vector<int>> x31 = x30;
      for (auto x32 = (x9[1]).begin(); x32 != (x9[1]).end(); ++x32) {
        std::vector<std::vector<int>> x33 = std::get<0>(*x32);
        // begin Array.init
        x34.clear();
        x34.reserve(((int)((x33).size())));
        for (int x35 = 0; x35 < ((int)((x33).size())); x35 += 1) {
          std::vector<int> x36 = (x33[x35]);
          int x37 = ((int)((x36).size()));
          // begin Array.init
          x38.clear();
          x38.reserve(x37);
          for (int x39 = 0; x39 < x37; x39 += 1) {
            int x40 = (x36[((x37 - x39) - 1)]);
            x38.push_back(x40);
          }
          // end Array.init
          x34.push_back(x38);
        }
        // end Array.init

        if ((0 || (x34 == x31))) {
          std::cout << "(App reverse ((App L0 ())))" << std::endl;
          reconstruct_L_1(std::get<0>(*x32));
          x43 = 0;
        } else {

          x43 = 0;
        }
      }
      return 0;
    }
    int reconstruct_L_1(const std::vector<std::vector<int>> &x41) {
      std::vector<std::vector<int>> x42 = x41;
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
