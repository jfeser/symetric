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
    int x4;
    int main() {
      // begin Array.init
      x0.clear();
      x0.reserve(10);
      for (int x1 = 0; x1 < 10; x1++) {
        int x2 = x1;
        x0.push_back(x2);
      }
      // end Array.init
      std::vector<int> x3 = x0;
      // begin Array.fold
      x4 = 0;
      int x5 = ((int)((x3).size()));
      for (int x6 = 0; x6 < x5; x6++) {
        x4 = (x4 + (x3[x6]));
      }
      // end Array.fold
      return x4;
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
    int g(const int &x1);
    int f(const int &x0);
    int main();
    int main() { return f(g(0)); }
    int f(const int &x0) { return (x0 + 1); }
    int g(const int &x1) { return (x1 - 1); } |}];
  code |> Util.clang_build |> print_endline;
  [%expect
    {| |}]

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
    int f(const std::vector<int> &x0);
    int main();
    std::vector<int> x1;
    int main() {
      // begin Array.init
      x1.clear();
      x1.reserve(10);
      for (int x2 = 0; x2 < 10; x2++) {
        int x3 = x2;
        x1.push_back(x3);
      }
      // end Array.init
      return f(x1);
    }
    int f(const std::vector<int> &x0) { return (x0[0]); } |}];
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
    std::vector<int> x3;
    int main() {
      // begin Array.init
      x3.clear();
      x3.reserve(10);
      for (int x4 = 0; x4 < 10; x4++) {
        int x5 = x4;
        x3.push_back(x5);
      }
      // end Array.init

      // begin Array.init
      x0.clear();
      x0.reserve(10);
      for (int x1 = 0; x1 < 10; x1++) {
        int x2 = x1;
        x0.push_back(x2);
      }
      // end Array.init

      // begin Array.init
      x3.clear();
      x3.reserve(10);
      for (int x4 = 0; x4 < 10; x4++) {
        int x5 = x4;
        x3.push_back(x5);
      }
      // end Array.init

      // begin Array.init
      x0.clear();
      x0.reserve(10);
      for (int x1 = 0; x1 < 10; x1++) {
        int x2 = x1;
        x0.push_back(x2);
      }
      // end Array.init
      return ((std::get<0>(std::make_pair(x3, x0))[5]) +
              (std::get<0>(std::make_pair(x3, x0))[4]));
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
    int x2;
    int main() {
      std::set<int> x1 = x0;
      x1.insert(0);
      x1.insert(1);
      x1.insert(2);
      // begin Set.fold
      int x3 = x2;
      x3 = 0;
      for (auto x4 = x1.begin(); x4 != x1.end(); ++x4) {
        x3 = (x3 + *x4);
      }
      // end Set.fold
      int x5 = x3;
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
    int x2;
    int main() {
      std::set<int> x1 = x0;
      x1.insert(0);
      x1.insert(1);
      x1.insert(2);
      // begin Set.fold
      int x3 = x2;
      x3 = 0;
      int x5 = (45 + 10);
      for (auto x4 = x1.begin(); x4 != x1.end(); ++x4) {
        x3 = ((x5 + x3) + *x4);
      }
      // end Set.fold
      int x6 = x3;
      return 0;
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
      std::pair<int, int> x0 = std::make_pair(0, 1);
      return (std::get<0>(x0) + std::get<0>(x0));
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let code = ite (bool true) (fun () -> int 0) (fun () -> int 1) in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int main();
    int x0;
    int main() {
      if (1) {

        x0 = 0;
      } else {

        x0 = 1;
      }
      return x0;
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let code =
    ite (bool true) (fun () -> seq (print "test") exit) (fun () -> unit)
  in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int main();
    int x0;
    int main() {
      if (1) {
        std::cout << "test" << std::endl;
        exit(0);
        x0 = 0;
      } else {

        x0 = 0;
      }
      return x0;
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let code =
    ite (bool true)
      (fun () -> int 0)
      (fun () ->
        let x = genlet (int 10) in
        x + int 4)
  in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int main();
    int x1;
    int main() {
      int x0 = 10;
      if (1) {

        x1 = 0;
      } else {

        x1 = (x0 + 4);
      }
      return x1;
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let code = for_ (int 0) (int 1) (int 10) (fun _ -> seq (print "test") exit) in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int main();
    int main() {
      for (int x0 = 0; x0 < 10; x0++) {
        std::cout << "test" << std::endl;
        exit(0);
      }
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
      (fun arr ->
        Array.fold arr ~init:(int 0) ~f:( + )
        + Array.fold arr ~init:(int 0) ~f:( * ))
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
    int x2;
    int x5;
    int main() {
      // begin Array.const
      x0[0] = 0;
      x0[1] = 1;
      x0[2] = 2; // end Array.const
      std::vector<int> x1 = x0;
      // begin Array.fold
      x5 = 0;
      int x6 = ((int)((x1).size()));
      for (int x7 = 0; x7 < x6; x7++) {
        x5 = (x5 + (x1[x7]));
      }
      // end Array.fold

      // begin Array.fold
      x2 = 0;
      int x3 = ((int)((x1).size()));
      for (int x4 = 0; x4 < x3; x4++) {
        x2 = (x2 * (x1[x4]));
      }
      // end Array.fold
      return (x5 + x2);
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let code =
    let arr = Array.const (Array.mk_type Int) [| int 0; int 1; int 2 |] in
    Array.fold arr ~init:(int 0) ~f:( + )
    + Array.fold arr ~init:(int 0) ~f:( * )
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
    int x4;
    int main() {
      // begin Array.fold
      x4 = 0;
      // begin Array.const
      x0[0] = 0;
      x0[1] = 1;
      x0[2] = 2; // end Array.const
      int x5 = ((int)((x0).size()));
      for (int x6 = 0; x6 < x5; x6++) {

        // begin Array.const
        x0[0] = 0;
        x0[1] = 1;
        x0[2] = 2; // end Array.const
        x4 = (x4 + (x0[x6]));
      }
      // end Array.fold

      // begin Array.fold
      x1 = 0;
      // begin Array.const
      x0[0] = 0;
      x0[1] = 1;
      x0[2] = 2; // end Array.const
      int x2 = ((int)((x0).size()));
      for (int x3 = 0; x3 < x2; x3++) {

        // begin Array.const
        x0[0] = 0;
        x0[1] = 1;
        x0[2] = 2; // end Array.const
        x1 = (x1 * (x0[x3]));
      }
      // end Array.fold
      return (x4 + x1);
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let code =
    let_locus @@ fun () ->
    let arr =
      genlet @@ Array.const (Array.mk_type Int) [| int 0; int 1; int 2 |]
    in
    Array.fold arr ~init:(int 0) ~f:( + )
    + Array.fold arr ~init:(int 0) ~f:( * )
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
    int x2;
    int x5;
    int main() {
      // begin Array.const
      x0[0] = 0;
      x0[1] = 1;
      x0[2] = 2; // end Array.const
      std::vector<int> x1 = x0;
      // begin Array.fold
      x5 = 0;
      int x6 = ((int)((x1).size()));
      for (int x7 = 0; x7 < x6; x7++) {
        x5 = (x5 + (x1[x7]));
      }
      // end Array.fold

      // begin Array.fold
      x2 = 0;
      int x3 = ((int)((x1).size()));
      for (int x4 = 0; x4 < x3; x4++) {
        x2 = (x2 * (x1[x4]));
      }
      // end Array.fold
      return (x5 + x2);
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let code =
    let_locus @@ fun () ->
    let arr () =
      genlet @@ Array.const (Array.mk_type Int) [| int 0; int 1; int 2 |]
    in
    Array.fold (arr ()) ~init:(int 0) ~f:( + )
    + Array.fold (arr ()) ~init:(int 0) ~f:( * )
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
    int x2;
    std::vector<int> x5(3);
    int x7;
    int main() {
      // begin Array.const
      x0[0] = 0;
      x0[1] = 1;
      x0[2] = 2; // end Array.const
      std::vector<int> x1 = x0;
      // begin Array.const
      x5[0] = 0;
      x5[1] = 1;
      x5[2] = 2; // end Array.const
      std::vector<int> x6 = x5;
      // begin Array.fold
      x7 = 0;
      int x8 = ((int)((x6).size()));
      for (int x9 = 0; x9 < x8; x9++) {
        x7 = (x7 + (x6[x9]));
      }
      // end Array.fold

      // begin Array.fold
      x2 = 0;
      int x3 = ((int)((x1).size()));
      for (int x4 = 0; x4 < x3; x4++) {
        x2 = (x2 * (x1[x4]));
      }
      // end Array.fold
      return (x7 + x2);
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let code =
    let arr =
      genlet @@ Array.const (Array.mk_type Int) [| int 0; int 1; int 2 |]
    in
    let_locus @@ fun () ->
    Array.fold arr ~init:(int 0) ~f:( + )
    + Array.fold arr ~init:(int 0) ~f:( * )
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
    int x4;
    int main() {
      // begin Array.fold
      x4 = 0;
      // begin Array.const
      x0[0] = 0;
      x0[1] = 1;
      x0[2] = 2; // end Array.const
      int x5 = ((int)((x0).size()));
      for (int x6 = 0; x6 < x5; x6++) {

        // begin Array.const
        x0[0] = 0;
        x0[1] = 1;
        x0[2] = 2; // end Array.const
        x4 = (x4 + (x0[x6]));
      }
      // end Array.fold

      // begin Array.fold
      x1 = 0;
      // begin Array.const
      x0[0] = 0;
      x0[1] = 1;
      x0[2] = 2; // end Array.const
      int x2 = ((int)((x0).size()));
      for (int x3 = 0; x3 < x2; x3++) {

        // begin Array.const
        x0[0] = 0;
        x0[1] = 1;
        x0[2] = 2; // end Array.const
        x1 = (x1 * (x0[x3]));
      }
      // end Array.fold
      return (x4 + x1);
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let arr =
    lazy (genlet @@ Array.const (Array.mk_type Int) [| int 0; int 1; int 2 |])
  in
  let code =
    let_locus @@ fun () ->
    Array.fold (Lazy.force arr) ~init:(int 0) ~f:( + )
    + Array.fold (Lazy.force arr) ~init:(int 0) ~f:( * )
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
    int x2;
    int x5;
    int main() {
      // begin Array.const
      x0[0] = 0;
      x0[1] = 1;
      x0[2] = 2; // end Array.const
      std::vector<int> x1 = x0;
      // begin Array.fold
      x5 = 0;
      int x6 = ((int)((x1).size()));
      for (int x7 = 0; x7 < x6; x7++) {
        x5 = (x5 + (x1[x7]));
      }
      // end Array.fold

      // begin Array.fold
      x2 = 0;
      int x3 = ((int)((x1).size()));
      for (int x4 = 0; x4 < x3; x4++) {
        x2 = (x2 * (x1[x4]));
      }
      // end Array.fold
      return (x5 + x2);
    }
 |}];
  code |> Util.clang_build |> print_endline
