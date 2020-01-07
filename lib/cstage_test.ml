open! Core
open! Cstage

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Array in
  let code =
    let_locus @@ fun () ->
    let_
      (init (mk_type Int) (int 10) (fun i -> i))
      (fun a -> fold a ~init:(int 0) ~f:( + ))
  in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int main();
    std::vector<int> x0;
    int x2;
    int main() {
      // begin Array.init
      x0.clear();
      x0.reserve(10);
      for (int x1 = 0; x1 < 10; x1++) {
        x0.push_back(x1);
      }
      // end Array.init

      // begin Array.fold
      x2 = 0;
      for (int x3 = 0; x3 < ((int)((x0).size())); x3++) {
        x2 = (x2 + (x0[x3]));
      }
      // end Array.fold
      return x2;
    } |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let f = func "f" (Func (Int, Int)) (fun i -> i + int 1) in
  let g = func "g" (Func (Int, Int)) (fun i -> i - int 1) in
  let code = apply f (apply g (int 0)) in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int g(int &x1);
    int f(int &x0);
    int main();
    int main() { return f(g(0)); }
    int f(int &x0) { return (x0 + 1); }
    int g(int &x1) { return (x1 - 1); } |}];
  code |> Util.clang_build |> print_endline;
  [%expect
    {|
    <stdin>:6:37: error: no matching function for call to 'g'
    int main();int main() {    return f(g(0)); }
                                        ^
    <stdin>:4:5: note: candidate function not viable: expects an l-value for 1st argument
    int g(int &x1);
        ^
    1 error generated. |}]

let%expect_test "" =
  let module C = Code () in
  let open C in
  let int_array = Array.mk_type Int in
  let f = func "f" (Func (int_array, Int)) (fun a -> a.(int 0)) in
  let code = apply f (Array.init int_array (int 10) (fun i -> i)) in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int f(std::vector<int> &x0);
    int main();
    std::vector<int> x1;
    int main() {
      // begin Array.init
      x1.clear();
      x1.reserve(10);
      for (int x2 = 0; x2 < 10; x2++) {
        x1.push_back(x2);
      }
      // end Array.init
      return f(x1);
    }
    int f(std::vector<int> &x0) { return (x0[0]); } |}];
  code |> Util.clang_build |> print_endline;
  [%expect {| |}]

let%expect_test "" =
  let module C = Code () in
  let open C in
  let int_array = Array.mk_type Int in
  let f =
    let x =
      Tuple.create
        (Array.init int_array (int 10) (fun i -> i))
        (Array.init int_array (int 10) (fun i -> i))
    in
    let_locus @@ fun () ->
    let y = genlet (Tuple.fst x) in
    y.(int 5) + y.(int 4)
  in
  let code = to_string f in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int main();
    std::vector<int> x0;
    std::vector<int> x2;
    int main() {
      // begin Array.init
      x2.clear();
      x2.reserve(10);
      for (int x3 = 0; x3 < 10; x3++) {
        x2.push_back(x3);
      }
      // end Array.init

      // begin Array.init
      x0.clear();
      x0.reserve(10);
      for (int x1 = 0; x1 < 10; x1++) {
        x0.push_back(x1);
      }
      // end Array.init

      // begin Array.init
      x2.clear();
      x2.reserve(10);
      for (int x3 = 0; x3 < 10; x3++) {
        x2.push_back(x3);
      }
      // end Array.init

      // begin Array.init
      x0.clear();
      x0.reserve(10);
      for (int x1 = 0; x1 < 10; x1++) {
        x0.push_back(x1);
      }
      // end Array.init
      return ((std::get<0>(std::make_pair(x2, x0))[5]) +
              (std::get<0>(std::make_pair(x2, x0))[4]));
    }
 |}];
  code |> Util.clang_build |> print_endline;
  [%expect {| |}]

let%expect_test "" =
  let module C = Code () in
  let open C in
  let code =
    let_
      (Set.empty (Set.mk_type Int))
      (fun set ->
        seq_many
          [
            Set.add set (int 0);
            Set.add set (int 1);
            Set.add set (int 2);
            let_ (Set.fold set ~init:(int 0) ~f:( + )) (fun _ -> unit);
          ])
  in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int main();
    std::set<int> x0;
    int x1;
    int main() {
      x0.insert(0);
      x0.insert(1);
      x0.insert(2);
      // begin Set.fold
      x1 = 0;
      for (auto x2 = x0.begin(); x2 != x0.end(); ++x2) {
        x1 = (x1 + *x2);
      }
      // end Set.fold
      return 0;
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let code =
    let_
      (Set.empty (Set.mk_type Int))
      (fun set ->
        seq_many
          [
            Set.add set (int 0);
            Set.add set (int 1);
            Set.add set (int 2);
            let_
              (Set.fold set ~init:(int 0) ~f:(fun x y ->
                   genlet (int 45 + int 10) + x + y))
              (fun _ -> unit);
          ])
  in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int main();
    std::set<int> x0;
    int x1;
    int main() {
      x0.insert(0);
      x0.insert(1);
      x0.insert(2);
      // begin Set.fold
      x1 = 0;
      for (auto x2 = x0.begin(); x2 != x0.end(); ++x2) {
        x1 = (((45 + 10) + x1) + *x2);
      }
      // end Set.fold
      return 0;
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let code =
    let_
      (Array.const (Array.mk_type Int) [| int 0; int 1; int 2 |])
      (fun arr -> Array.fold arr ~init:(int 0) ~f:( + ))
  in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int main();
    std::vector<int> x0(3);
    int x1;
    int main() {
      // begin Array.const
      x0[0] = 0;
      x0[1] = 1;
      x0[2] = 2; // end Array.const

      // begin Array.fold
      x1 = 0;
      for (int x2 = 0; x2 < ((int)((x0).size())); x2++) {
        x1 = (x1 + (x0[x2]));
      }
      // end Array.fold
      return x1;
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let code =
    let_
      (Tuple.create (int 0) (int 1))
      (fun t ->
        let x = Tuple.fst t in
        x + x)
  in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int main();
    int main() {
      return (std::get<0>(std::make_pair(0, 1)) +
              std::get<0>(std::make_pair(0, 1)));
    }
 |}];
  code |> Util.clang_build |> print_endline
