open! Core
open! Cstage

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Array in
  let open Int in
  let code =
    let_locus @@ fun () ->
    let_
      (init (mk_type int_t) (int 10) (fun i -> i))
      (fun a -> fold a ~init:(int 0) ~f:( + ))
  in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>

    #include "sexp.hpp"
    int main();
    std::vector<int> x2;
    int x6;
    int main() {
      // begin Array.init
      x2.clear();
      x2.reserve(10);
      for (int x3 = 0; x3 < 10; x3 += 1) {
        int x4 = x3;
        x2.push_back(x4);
      }
      // end Array.init
      std::vector<int> x5 = x2;
      // begin Array.fold
      x6 = 0;
      int x7 = ((int)((x5).size()));
      for (int x8 = 0; x8 < x7; x8 += 1) {
        x6 = (x6 + (x5[x8]));
      }
      // end Array.fold
      return x6;
    } |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Int in
  let f = func "f" (Func.type_ int_t int_t) (fun i -> i + int 1) in
  let g = func "g" (Func.type_ int_t int_t) (fun i -> i - int 1) in
  let code = apply f (apply g (int 0)) in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>

    #include "sexp.hpp"
    int g(const int &x3);
    int f(const int &x2);
    int main();
    int main() { return f(g(0)); }
    int f(const int &x2) { return (x2 + 1); }
    int g(const int &x3) { return (x3 - 1); } |}];
  code |> Util.clang_build |> print_endline;
  [%expect {| |}]

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Int in
  let int_array = Array.mk_type int_t in
  let f = func "f" (Func.type_ int_array int_t) (fun a -> a.(int 0)) in
  let code = apply f (Array.init int_array (int 10) (fun i -> i)) in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>

    #include "sexp.hpp"
    int f(const std::vector<int> &x2);
    int main();
    std::vector<int> x3;
    int main() {
      // begin Array.init
      x3.clear();
      x3.reserve(10);
      for (int x4 = 0; x4 < 10; x4 += 1) {
        int x5 = x4;
        x3.push_back(x5);
      }
      // end Array.init
      return f(x3);
    }
    int f(const std::vector<int> &x2) { return (x2[0]); } |}];
  code |> Util.clang_build |> print_endline;
  [%expect {| |}]

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Int in
  let int_array = Array.mk_type int_t in
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

    #include "sexp.hpp"
    int main();
    std::vector<int> x2;
    std::vector<int> x5;
    int main() {
      // begin Array.init
      x5.clear();
      x5.reserve(10);
      for (int x6 = 0; x6 < 10; x6 += 1) {
        int x7 = x6;
        x5.push_back(x7);
      }
      // end Array.init

      // begin Array.init
      x2.clear();
      x2.reserve(10);
      for (int x3 = 0; x3 < 10; x3 += 1) {
        int x4 = x3;
        x2.push_back(x4);
      }
      // end Array.init

      // begin Array.init
      x5.clear();
      x5.reserve(10);
      for (int x6 = 0; x6 < 10; x6 += 1) {
        int x7 = x6;
        x5.push_back(x7);
      }
      // end Array.init

      // begin Array.init
      x2.clear();
      x2.reserve(10);
      for (int x3 = 0; x3 < 10; x3 += 1) {
        int x4 = x3;
        x2.push_back(x4);
      }
      // end Array.init
      return ((std::get<0>(std::make_pair(x5, x2))[5]) +
              (std::get<0>(std::make_pair(x5, x2))[4]));
    }
 |}];
  code |> Util.clang_build |> print_endline;
  [%expect {| |}]

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Int in
  let code =
    let_
      (Set.empty (Set.mk_type int_t))
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

    #include "sexp.hpp"
    int main();
    std::set<int> x2;
    int x4;
    int main() {
      std::set<int> x3 = x2;
      x3.insert(0);
      x3.insert(1);
      x3.insert(2);
      // begin Set.fold
      int x5 = x4;
      x5 = 0;
      for (auto x6 = x3.begin(); x6 != x3.end(); ++x6) {
        x5 = (x5 + *x6);
      }
      // end Set.fold
      int x7 = x5;
      return 0;
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Int in
  let code =
    let_
      (Set.empty (Set.mk_type int_t))
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

    #include "sexp.hpp"
    int main();
    std::set<int> x2;
    int x4;
    int main() {
      std::set<int> x3 = x2;
      x3.insert(0);
      x3.insert(1);
      x3.insert(2);
      // begin Set.fold
      int x5 = x4;
      x5 = 0;
      int x7 = (45 + 10);
      for (auto x6 = x3.begin(); x6 != x3.end(); ++x6) {
        x5 = ((x7 + x5) + *x6);
      }
      // end Set.fold
      int x8 = x5;
      return 0;
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Int in
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

    #include "sexp.hpp"
    int main();
    int main() {
      std::pair<int, int> x2 = std::make_pair(0, 1);
      return (std::get<0>(x2) + std::get<0>(x2));
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Int in
  let open Bool in
  let code = ite (bool true) (fun () -> int 0) (fun () -> int 1) in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>

    #include "sexp.hpp"
    int main();
    int x2;
    int main() {
      if (1) {

        x2 = 0;
      } else {

        x2 = 1;
      }
      return x2;
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Bool in
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

    #include "sexp.hpp"
    int main();
    int x2;
    int main() {
      if (1) {
        std::cout << "test" << std::endl;
        exit(0);
        x2 = 0;
      } else {

        x2 = 0;
      }
      return x2;
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Int in
  let open Bool in
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

    #include "sexp.hpp"
    int main();
    int x3;
    int main() {
      int x2 = 10;
      if (1) {

        x3 = 0;
      } else {

        x3 = (x2 + 4);
      }
      return x3;
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Int in
  let code = for_ (int 0) (int 1) (int 10) (fun _ -> seq (print "test") exit) in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>

    #include "sexp.hpp"
    int main();
    int main() {
      for (int x2 = 0; x2 < 10; x2 += 1) {
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
  let open Int in
  let code =
    let_
      (Array.const (Array.mk_type int_t) [| int 0; int 1; int 2 |])
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

    #include "sexp.hpp"
    int main();
    std::vector<int> x2(3);
    int x4;
    int x7;
    int main() {
      // begin Array.const
      x2[0] = 0;
      x2[1] = 1;
      x2[2] = 2; // end Array.const
      std::vector<int> x3 = x2;
      // begin Array.fold
      x7 = 0;
      int x8 = ((int)((x3).size()));
      for (int x9 = 0; x9 < x8; x9 += 1) {
        x7 = (x7 + (x3[x9]));
      }
      // end Array.fold

      // begin Array.fold
      x4 = 0;
      int x5 = ((int)((x3).size()));
      for (int x6 = 0; x6 < x5; x6 += 1) {
        x4 = (x4 * (x3[x6]));
      }
      // end Array.fold
      return (x7 + x4);
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Int in
  let code =
    let arr = Array.const (Array.mk_type int_t) [| int 0; int 1; int 2 |] in
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

    #include "sexp.hpp"
    int main();
    std::vector<int> x2(3);
    int x3;
    int x6;
    int main() {
      // begin Array.fold
      x6 = 0;
      // begin Array.const
      x2[0] = 0;
      x2[1] = 1;
      x2[2] = 2; // end Array.const
      int x7 = ((int)((x2).size()));
      for (int x8 = 0; x8 < x7; x8 += 1) {

        // begin Array.const
        x2[0] = 0;
        x2[1] = 1;
        x2[2] = 2; // end Array.const
        x6 = (x6 + (x2[x8]));
      }
      // end Array.fold

      // begin Array.fold
      x3 = 0;
      // begin Array.const
      x2[0] = 0;
      x2[1] = 1;
      x2[2] = 2; // end Array.const
      int x4 = ((int)((x2).size()));
      for (int x5 = 0; x5 < x4; x5 += 1) {

        // begin Array.const
        x2[0] = 0;
        x2[1] = 1;
        x2[2] = 2; // end Array.const
        x3 = (x3 * (x2[x5]));
      }
      // end Array.fold
      return (x6 + x3);
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Int in
  let code =
    let_locus @@ fun () ->
    let arr =
      genlet @@ Array.const (Array.mk_type int_t) [| int 0; int 1; int 2 |]
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

    #include "sexp.hpp"
    int main();
    std::vector<int> x2(3);
    int x4;
    int x7;
    int main() {
      // begin Array.const
      x2[0] = 0;
      x2[1] = 1;
      x2[2] = 2; // end Array.const
      std::vector<int> x3 = x2;
      // begin Array.fold
      x7 = 0;
      int x8 = ((int)((x3).size()));
      for (int x9 = 0; x9 < x8; x9 += 1) {
        x7 = (x7 + (x3[x9]));
      }
      // end Array.fold

      // begin Array.fold
      x4 = 0;
      int x5 = ((int)((x3).size()));
      for (int x6 = 0; x6 < x5; x6 += 1) {
        x4 = (x4 * (x3[x6]));
      }
      // end Array.fold
      return (x7 + x4);
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Int in
  let code =
    let_locus @@ fun () ->
    let arr () =
      genlet @@ Array.const (Array.mk_type int_t) [| int 0; int 1; int 2 |]
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

    #include "sexp.hpp"
    int main();
    std::vector<int> x2(3);
    int x4;
    std::vector<int> x7(3);
    int x9;
    int main() {
      // begin Array.const
      x2[0] = 0;
      x2[1] = 1;
      x2[2] = 2; // end Array.const
      std::vector<int> x3 = x2;
      // begin Array.const
      x7[0] = 0;
      x7[1] = 1;
      x7[2] = 2; // end Array.const
      std::vector<int> x8 = x7;
      // begin Array.fold
      x9 = 0;
      int x10 = ((int)((x8).size()));
      for (int x11 = 0; x11 < x10; x11 += 1) {
        x9 = (x9 + (x8[x11]));
      }
      // end Array.fold

      // begin Array.fold
      x4 = 0;
      int x5 = ((int)((x3).size()));
      for (int x6 = 0; x6 < x5; x6 += 1) {
        x4 = (x4 * (x3[x6]));
      }
      // end Array.fold
      return (x9 + x4);
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Int in
  let code =
    let arr =
      genlet @@ Array.const (Array.mk_type int_t) [| int 0; int 1; int 2 |]
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

    #include "sexp.hpp"
    int main();
    std::vector<int> x2(3);
    int x3;
    int x6;
    int main() {
      // begin Array.fold
      x6 = 0;
      // begin Array.const
      x2[0] = 0;
      x2[1] = 1;
      x2[2] = 2; // end Array.const
      int x7 = ((int)((x2).size()));
      for (int x8 = 0; x8 < x7; x8 += 1) {

        // begin Array.const
        x2[0] = 0;
        x2[1] = 1;
        x2[2] = 2; // end Array.const
        x6 = (x6 + (x2[x8]));
      }
      // end Array.fold

      // begin Array.fold
      x3 = 0;
      // begin Array.const
      x2[0] = 0;
      x2[1] = 1;
      x2[2] = 2; // end Array.const
      int x4 = ((int)((x2).size()));
      for (int x5 = 0; x5 < x4; x5 += 1) {

        // begin Array.const
        x2[0] = 0;
        x2[1] = 1;
        x2[2] = 2; // end Array.const
        x3 = (x3 * (x2[x5]));
      }
      // end Array.fold
      return (x6 + x3);
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Int in
  let arr =
    lazy (genlet @@ Array.const (Array.mk_type int_t) [| int 0; int 1; int 2 |])
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

    #include "sexp.hpp"
    int main();
    std::vector<int> x2(3);
    int x4;
    int x7;
    int main() {
      // begin Array.const
      x2[0] = 0;
      x2[1] = 1;
      x2[2] = 2; // end Array.const
      std::vector<int> x3 = x2;
      // begin Array.fold
      x7 = 0;
      int x8 = ((int)((x3).size()));
      for (int x9 = 0; x9 < x8; x9 += 1) {
        x7 = (x7 + (x3[x9]));
      }
      // end Array.fold

      // begin Array.fold
      x4 = 0;
      int x5 = ((int)((x3).size()));
      for (int x6 = 0; x6 < x5; x6 += 1) {
        x4 = (x4 * (x3[x6]));
      }
      // end Array.fold
      return (x7 + x4);
    }
 |}];
  code |> Util.clang_build |> print_endline

let%expect_test "" =
  let module C = Code () in
  let open C in
  let code =
    let_ Sexp.input @@ fun sexp ->
    let_ (Array.of_sexp (Array.mk_type String.type_) sexp String.of_sexp)
    @@ fun arr -> Array.iter arr ~f:(fun str -> String.print str)
  in
  let code = to_string code in
  code |> Util.clang_format |> print_endline;
  [%expect {|
    #include <iostream>
    #include <set>
    #include <vector>

    #include "sexp.hpp"
    int main();
    std::vector<std::string> x4;
    int main() {
      const std::unique_ptr<sexp> &x2 = sexp::load(std::cin);
      const std::vector<std::unique_ptr<sexp>> &x3 = ((list *)x2.get())->get_body();
      // begin Array.init
      x4.clear();
      x4.reserve((int)((x3).size()));
      for (int x5 = 0; x5 < (int)((x3).size()); x5 += 1) {
        x4.push_back(((atom *)(x3)[x5].get())->get_body());
      }
      // end Array.init
      std::vector<std::string> x6 = x4;
      // begin Array.iter
      int x7 = ((int)((x6).size()));
      for (int x8 = 0; x8 < x7; x8 += 1) {
        std::cout << (x6[x8]) << std::endl;
      }
      // end Array.iter
      return 0;
    } |}];
  let out = Util.clang_exec ~input:"(one two three)" code in
  out#exe_output |> print_endline;
  [%expect {|
    one
    two
    three |}]
