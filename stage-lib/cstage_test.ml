(* open! Core
 * open! Cstage
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Array in
 *   let open Int in
 *   let code =
 *     let_locus @@ fun () ->
 *     let_ (init (int 10) (fun i -> i)) (fun a -> fold a ~init:(int 0) ~f:( + ))
 *   in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int main() {
 *       // begin Array.init
 *       std::vector<int32_t> x2(10);
 *       std::vector<int32_t> x3 = x2;
 *       for (int x4 = 0; x4 < 10; x4 += 1) {
 *         int x5 = x4;
 *         x3[x4] = x5;
 *       }
 *       // end Array.init
 *       std::vector<int32_t> x6 = x3;
 *       // begin Array.fold
 *       int32_t x7(0);
 *       int32_t x8 = x7;
 *       int32_t x9 = ((int)((x6).size()));
 *       for (int x10 = 0; x10 < x9; x10 += 1) {
 *         x8 = (x8 + (x6[x10]));
 *       }
 *       // end Array.fold
 *       return x8;
 *     } |}];
 *   code |> Util.clang_build |> print_endline
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Int in
 *   let int_t = C.Int.type_ in
 *   let open Func in
 *   let f = func "f" (mk_type int_t int_t) (fun i -> i + int 1) in
 *   let g = func "g" (mk_type int_t int_t) (fun i -> i - int 1) in
 *   let code = apply f (apply g (int 0)) in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int32_t g(int32_t x3);
 *     int32_t f(int32_t x2);
 *     int main();
 *     int main() { return f(g(0)); }
 *     int32_t f(int32_t x2) { return (x2 + 1); }
 *     int32_t g(int32_t x3) { return (x3 - 1); } |}];
 *   code |> Util.clang_build |> print_endline;
 *   [%expect {| |}]
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Int in
 *   let open Func in
 *   let int_t = C.Int.type_ in
 *   let int_array = Array.mk_type int_t in
 *   let f = func "f" (mk_type int_array int_t) (fun a -> a.(int 0)) in
 *   let code = apply f (Array.init (int 10) (fun i -> i)) in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int32_t f(std::vector<int32_t> x2);
 *     int main();
 *     int main() {
 *       // begin Array.init
 *       std::vector<int32_t> x3(10);
 *       std::vector<int32_t> x4 = x3;
 *       for (int x5 = 0; x5 < 10; x5 += 1) {
 *         int x6 = x5;
 *         x4[x5] = x6;
 *       }
 *       // end Array.init
 *       return f(x4);
 *     }
 *     int32_t f(std::vector<int32_t> x2) { return (x2[0]); } |}];
 *   code |> Util.clang_build |> print_endline;
 *   [%expect {| |}]
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Int in
 *   let f =
 *     let x =
 *       Tuple.create
 *         (Array.init (int 10) (fun i -> i))
 *         (Array.init (int 10) (fun i -> i))
 *     in
 *     let_locus @@ fun () ->
 *     let y = genlet (Tuple.fst x) in
 *     y.(int 5) + y.(int 4)
 *   in
 *   let code = to_string f in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int main() {
 *       // begin Array.init
 *       std::vector<int32_t> x6(10);
 *       std::vector<int32_t> x7 = x6;
 *       for (int x8 = 0; x8 < 10; x8 += 1) {
 *         int x9 = x8;
 *         x7[x8] = x9;
 *       }
 *       // end Array.init
 * 
 *       // begin Array.init
 *       std::vector<int32_t> x2(10);
 *       std::vector<int32_t> x3 = x2;
 *       for (int x4 = 0; x4 < 10; x4 += 1) {
 *         int x5 = x4;
 *         x3[x4] = x5;
 *       }
 *       // end Array.init
 * 
 *       // begin Array.init
 *       std::vector<int32_t> x6(10);
 *       std::vector<int32_t> x7 = x6;
 *       for (int x8 = 0; x8 < 10; x8 += 1) {
 *         int x9 = x8;
 *         x7[x8] = x9;
 *       }
 *       // end Array.init
 * 
 *       // begin Array.init
 *       std::vector<int32_t> x2(10);
 *       std::vector<int32_t> x3 = x2;
 *       for (int x4 = 0; x4 < 10; x4 += 1) {
 *         int x5 = x4;
 *         x3[x4] = x5;
 *       }
 *       // end Array.init
 *       return ((std::get<0>(std::make_pair(x7, x3))[5]) +
 *               (std::get<0>(std::make_pair(x7, x3))[4]));
 *     }
 *  |}];
 *   code |> Util.clang_build |> print_endline;
 *   [%expect
 *     {|
 *     <stdin>:78:24: error: redefinition of 'x6'
 *       std::vector<int32_t> x6 (10);  std::vector<int32_t> x7 = x6;
 *                            ^
 *     <stdin>:64:24: note: previous definition is here
 *       std::vector<int32_t> x6 (10);  std::vector<int32_t> x7 = x6;
 *                            ^
 *     <stdin>:78:55: error: redefinition of 'x7'
 *       std::vector<int32_t> x6 (10);  std::vector<int32_t> x7 = x6;
 *                                                           ^
 *     <stdin>:64:55: note: previous definition is here
 *       std::vector<int32_t> x6 (10);  std::vector<int32_t> x7 = x6;
 *                                                           ^
 *     <stdin>:85:24: error: redefinition of 'x2'
 *       std::vector<int32_t> x2 (10);  std::vector<int32_t> x3 = x2;
 *                            ^
 *     <stdin>:71:24: note: previous definition is here
 *       std::vector<int32_t> x2 (10);  std::vector<int32_t> x3 = x2;
 *                            ^
 *     <stdin>:85:55: error: redefinition of 'x3'
 *       std::vector<int32_t> x2 (10);  std::vector<int32_t> x3 = x2;
 *                                                           ^
 *     <stdin>:71:55: note: previous definition is here
 *       std::vector<int32_t> x2 (10);  std::vector<int32_t> x3 = x2;
 *                                                           ^
 *     4 errors generated. |}]
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Int in
 *   let code =
 *     let_
 *       (Set.empty (Set.mk_type Int.type_))
 *       (fun set ->
 *         sseq
 *           [
 *             Set.add set (int 0);
 *             Set.add set (int 1);
 *             Set.add set (int 2);
 *             let_ (Set.fold set ~init:(int 0) ~f:( + )) (fun _ -> unit);
 *           ])
 *   in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int32_t x4;
 *     int main() {
 *       std::set<int32_t> x2;
 *       std::set<int32_t> x3 = x2;
 *       x3.insert(0);
 *       x3.insert(1);
 *       x3.insert(2);
 *       // begin Set.fold
 *       int32_t x5 = x4;
 *       x5 = 0;
 *       for (auto x6 = x3.begin(); x6 != x3.end(); ++x6) {
 *         x5 = (x5 + (\*x6));
 *       }
 *       // end Set.fold
 *       int32_t x7 = x5;
 *       return 0;
 *     }
 *  |}];
 *   code |> Util.clang_build |> print_endline;
 *   [%expect
 *     {|
 *     <stdin>:69:11: warning: unused variable 'x7' [-Wunused-variable]
 *       int32_t x7 = x5; return 0; }
 *               ^
 *     1 warning generated. |}]
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Int in
 *   let code =
 *     let_
 *       (Set.empty (Set.mk_type Int.type_))
 *       (fun set ->
 *         sseq
 *           [
 *             Set.add set (int 0);
 *             Set.add set (int 1);
 *             Set.add set (int 2);
 *             let_
 *               (Set.fold set ~init:(int 0) ~f:(fun x y ->
 *                    genlet (int 45 + int 10) + x + y))
 *               (fun _ -> unit);
 *           ])
 *   in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int32_t x4;
 *     int main() {
 *       std::set<int32_t> x2;
 *       std::set<int32_t> x3 = x2;
 *       x3.insert(0);
 *       x3.insert(1);
 *       x3.insert(2);
 *       // begin Set.fold
 *       int32_t x5 = x4;
 *       x5 = 0;
 *       int32_t x7 = (45 + 10);
 *       for (auto x6 = x3.begin(); x6 != x3.end(); ++x6) {
 *         x5 = ((x7 + x5) + (\*x6));
 *       }
 *       // end Set.fold
 *       int32_t x8 = x5;
 *       return 0;
 *     }
 *  |}];
 *   code |> Util.clang_build |> print_endline;
 *   [%expect
 *     {|
 *     <stdin>:69:11: warning: unused variable 'x8' [-Wunused-variable]
 *       int32_t x8 = x5; return 0; }
 *               ^
 *     1 warning generated. |}]
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Int in
 *   let code =
 *     let_
 *       (Tuple.create (int 0) (int 1))
 *       (fun t ->
 *         let x = Tuple.fst t in
 *         x + x)
 *   in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int main() {
 *       std::pair<int32_t, int32_t> x2 = std::make_pair(0, 1);
 *       return (std::get<0>(x2) + std::get<0>(x2));
 *     }
 *  |}];
 *   code |> Util.clang_build |> print_endline
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Int in
 *   let open Bool in
 *   let code = ite (bool true) (fun () -> int 0) (fun () -> int 1) in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int main() {
 *       int32_t x2;
 *       int32_t x3 = x2;
 *       if (1) {
 * 
 *         x3 = 0;
 *       } else {
 * 
 *         x3 = 1;
 *       }
 *       return x3;
 *     }
 *  |}];
 *   code |> Util.clang_build |> print_endline;
 *   [%expect
 *     {|
 *     <stdin>:62:53: warning: variable 'x2' is uninitialized when used here [-Wuninitialized]
 *     int main();int main() {   int32_t x2;  int32_t x3 = x2;
 *                                                         ^~
 *     <stdin>:62:37: note: initialize the variable 'x2' to silence this warning
 *     int main();int main() {   int32_t x2;  int32_t x3 = x2;
 *                                         ^
 *                                          = 0
 *     1 warning generated. |}]
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Bool in
 *   let code =
 *     ite (bool true) (fun () -> seq (print "test") exit) (fun () -> unit)
 *   in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int main() {
 *       int x2;
 *       int x3 = x2;
 *       if (1) {
 *         std::cout << "test";
 *         exit(0);
 *         x3 = 0;
 *       } else {
 * 
 *         x3 = 0;
 *       }
 *       return x3;
 *     }
 *  |}];
 *   code |> Util.clang_build |> print_endline;
 *   [%expect
 *     {|
 *     <stdin>:62:45: warning: variable 'x2' is uninitialized when used here [-Wuninitialized]
 *     int main();int main() {   int x2;  int x3 = x2;
 *                                                 ^~
 *     <stdin>:62:33: note: initialize the variable 'x2' to silence this warning
 *     int main();int main() {   int x2;  int x3 = x2;
 *                                     ^
 *                                      = 0
 *     1 warning generated. |}]
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Int in
 *   let open Bool in
 *   let code =
 *     ite (bool true)
 *       (fun () -> int 0)
 *       (fun () ->
 *         let x = genlet (int 10) in
 *         x + int 4)
 *   in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int main() {
 *       int32_t x2 = 10;
 *       int32_t x3;
 *       int32_t x4 = x3;
 *       if (1) {
 * 
 *         x4 = 0;
 *       } else {
 * 
 *         x4 = (x2 + 4);
 *       }
 *       return x4;
 *     }
 *  |}];
 *   code |> Util.clang_build |> print_endline;
 *   [%expect
 *     {|
 *     <stdin>:62:71: warning: variable 'x3' is uninitialized when used here [-Wuninitialized]
 *     int main();int main() {    int32_t x2 = 10; int32_t x3;  int32_t x4 = x3;
 *                                                                           ^~
 *     <stdin>:62:55: note: initialize the variable 'x3' to silence this warning
 *     int main();int main() {    int32_t x2 = 10; int32_t x3;  int32_t x4 = x3;
 *                                                           ^
 *                                                            = 0
 *     1 warning generated. |}]
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Int in
 *   let code = for_ (int 0) (int 1) (int 10) (fun _ -> seq (print "test") exit) in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int main() {
 *       for (int x2 = 0; x2 < 10; x2 += 1) {
 *         std::cout << "test";
 *         exit(0);
 *       }
 *       return 0;
 *     }
 *  |}];
 *   code |> Util.clang_build |> print_endline
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Int in
 *   let code =
 *     let_
 *       (Array.const (Array.mk_type Int.type_) [| int 0; int 1; int 2 |])
 *       (fun arr ->
 *         Array.fold arr ~init:(int 0) ~f:( + )
 *         + Array.fold arr ~init:(int 0) ~f:( * ))
 *   in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int main() {
 *       // begin Array.const
 *       std::vector<int32_t> x2(3);
 *       std::vector<int32_t> x3 = x2;
 *       x3[0] = 0;
 *       x3[1] = 1;
 *       x3[2] = 2; // end Array.const
 *       std::vector<int32_t> x4 = x3;
 *       // begin Array.fold
 *       int32_t x9(0);
 *       int32_t x10 = x9;
 *       int32_t x11 = ((int)((x4).size()));
 *       for (int x12 = 0; x12 < x11; x12 += 1) {
 *         x10 = (x10 + (x4[x12]));
 *       }
 *       // end Array.fold
 * 
 *       // begin Array.fold
 *       int32_t x5(0);
 *       int32_t x6 = x5;
 *       int32_t x7 = ((int)((x4).size()));
 *       for (int x8 = 0; x8 < x7; x8 += 1) {
 *         x6 = (x6 * (x4[x8]));
 *       }
 *       // end Array.fold
 *       return (x10 + x6);
 *     }
 *  |}];
 *   code |> Util.clang_build |> print_endline
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Int in
 *   let code =
 *     let arr = Array.const (Array.mk_type Int.type_) [| int 0; int 1; int 2 |] in
 *     Array.fold arr ~init:(int 0) ~f:( + )
 *     + Array.fold arr ~init:(int 0) ~f:( * )
 *   in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int main() {
 *       // begin Array.fold
 *       int32_t x8(0);
 *       int32_t x9 = x8;
 *       // begin Array.const
 *       std::vector<int32_t> x2(3);
 *       std::vector<int32_t> x3 = x2;
 *       x3[0] = 0;
 *       x3[1] = 1;
 *       x3[2] = 2; // end Array.const
 *       int32_t x10 = ((int)((x3).size()));
 *       for (int x11 = 0; x11 < x10; x11 += 1) {
 * 
 *         // begin Array.const
 *         std::vector<int32_t> x2(3);
 *         std::vector<int32_t> x3 = x2;
 *         x3[0] = 0;
 *         x3[1] = 1;
 *         x3[2] = 2; // end Array.const
 *         x9 = (x9 + (x3[x11]));
 *       }
 *       // end Array.fold
 * 
 *       // begin Array.fold
 *       int32_t x4(0);
 *       int32_t x5 = x4;
 *       // begin Array.const
 *       std::vector<int32_t> x2(3);
 *       std::vector<int32_t> x3 = x2;
 *       x3[0] = 0;
 *       x3[1] = 1;
 *       x3[2] = 2; // end Array.const
 *       int32_t x6 = ((int)((x3).size()));
 *       for (int x7 = 0; x7 < x6; x7 += 1) {
 * 
 *         // begin Array.const
 *         std::vector<int32_t> x2(3);
 *         std::vector<int32_t> x3 = x2;
 *         x3[0] = 0;
 *         x3[1] = 1;
 *         x3[2] = 2; // end Array.const
 *         x5 = (x5 * (x3[x7]));
 *       }
 *       // end Array.fold
 *       return (x9 + x5);
 *     }
 *  |}];
 *   code |> Util.clang_build |> print_endline;
 *   [%expect
 *     {|
 *     <stdin>:79:24: error: redefinition of 'x2'
 *       std::vector<int32_t> x2 (3);  std::vector<int32_t> x3 = x2;  x3[0] = 0;  x3[1] = 1;  x3[2] = 2;// end Array.const
 *                            ^
 *     <stdin>:66:24: note: previous definition is here
 *       std::vector<int32_t> x2 (3);  std::vector<int32_t> x3 = x2;  x3[0] = 0;  x3[1] = 1;  x3[2] = 2;// end Array.const
 *                            ^
 *     <stdin>:79:54: error: redefinition of 'x3'
 *       std::vector<int32_t> x2 (3);  std::vector<int32_t> x3 = x2;  x3[0] = 0;  x3[1] = 1;  x3[2] = 2;// end Array.const
 *                                                          ^
 *     <stdin>:66:54: note: previous definition is here
 *       std::vector<int32_t> x2 (3);  std::vector<int32_t> x3 = x2;  x3[0] = 0;  x3[1] = 1;  x3[2] = 2;// end Array.const
 *                                                          ^
 *     2 errors generated. |}]
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Int in
 *   let code =
 *     let_locus @@ fun () ->
 *     let arr =
 *       genlet @@ Array.const (Array.mk_type Int.type_) [| int 0; int 1; int 2 |]
 *     in
 *     Array.fold arr ~init:(int 0) ~f:( + )
 *     + Array.fold arr ~init:(int 0) ~f:( * )
 *   in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int main() {
 *       // begin Array.const
 *       std::vector<int32_t> x2(3);
 *       std::vector<int32_t> x3 = x2;
 *       x3[0] = 0;
 *       x3[1] = 1;
 *       x3[2] = 2; // end Array.const
 *       std::vector<int32_t> x4 = x3;
 *       // begin Array.fold
 *       int32_t x9(0);
 *       int32_t x10 = x9;
 *       int32_t x11 = ((int)((x4).size()));
 *       for (int x12 = 0; x12 < x11; x12 += 1) {
 *         x10 = (x10 + (x4[x12]));
 *       }
 *       // end Array.fold
 * 
 *       // begin Array.fold
 *       int32_t x5(0);
 *       int32_t x6 = x5;
 *       int32_t x7 = ((int)((x4).size()));
 *       for (int x8 = 0; x8 < x7; x8 += 1) {
 *         x6 = (x6 * (x4[x8]));
 *       }
 *       // end Array.fold
 *       return (x10 + x6);
 *     }
 *  |}];
 *   code |> Util.clang_build |> print_endline;
 *   [%expect {| |}]
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Int in
 *   let code =
 *     let_locus @@ fun () ->
 *     let arr () =
 *       genlet @@ Array.const (Array.mk_type Int.type_) [| int 0; int 1; int 2 |]
 *     in
 *     Array.fold (arr ()) ~init:(int 0) ~f:( + )
 *     + Array.fold (arr ()) ~init:(int 0) ~f:( * )
 *   in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int main() {
 *       // begin Array.const
 *       std::vector<int32_t> x2(3);
 *       std::vector<int32_t> x3 = x2;
 *       x3[0] = 0;
 *       x3[1] = 1;
 *       x3[2] = 2; // end Array.const
 *       std::vector<int32_t> x4 = x3;
 *       // begin Array.const
 *       std::vector<int32_t> x9(3);
 *       std::vector<int32_t> x10 = x9;
 *       x10[0] = 0;
 *       x10[1] = 1;
 *       x10[2] = 2; // end Array.const
 *       std::vector<int32_t> x11 = x10;
 *       // begin Array.fold
 *       int32_t x12(0);
 *       int32_t x13 = x12;
 *       int32_t x14 = ((int)((x11).size()));
 *       for (int x15 = 0; x15 < x14; x15 += 1) {
 *         x13 = (x13 + (x11[x15]));
 *       }
 *       // end Array.fold
 * 
 *       // begin Array.fold
 *       int32_t x5(0);
 *       int32_t x6 = x5;
 *       int32_t x7 = ((int)((x4).size()));
 *       for (int x8 = 0; x8 < x7; x8 += 1) {
 *         x6 = (x6 * (x4[x8]));
 *       }
 *       // end Array.fold
 *       return (x13 + x6);
 *     }
 *  |}];
 *   code |> Util.clang_build |> print_endline
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Int in
 *   let code =
 *     let arr =
 *       genlet @@ Array.const (Array.mk_type Int.type_) [| int 0; int 1; int 2 |]
 *     in
 *     let_locus @@ fun () ->
 *     Array.fold arr ~init:(int 0) ~f:( + )
 *     + Array.fold arr ~init:(int 0) ~f:( * )
 *   in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int main() {
 *       // begin Array.fold
 *       int32_t x8(0);
 *       int32_t x9 = x8;
 *       // begin Array.const
 *       std::vector<int32_t> x2(3);
 *       std::vector<int32_t> x3 = x2;
 *       x3[0] = 0;
 *       x3[1] = 1;
 *       x3[2] = 2; // end Array.const
 *       int32_t x10 = ((int)((x3).size()));
 *       for (int x11 = 0; x11 < x10; x11 += 1) {
 * 
 *         // begin Array.const
 *         std::vector<int32_t> x2(3);
 *         std::vector<int32_t> x3 = x2;
 *         x3[0] = 0;
 *         x3[1] = 1;
 *         x3[2] = 2; // end Array.const
 *         x9 = (x9 + (x3[x11]));
 *       }
 *       // end Array.fold
 * 
 *       // begin Array.fold
 *       int32_t x4(0);
 *       int32_t x5 = x4;
 *       // begin Array.const
 *       std::vector<int32_t> x2(3);
 *       std::vector<int32_t> x3 = x2;
 *       x3[0] = 0;
 *       x3[1] = 1;
 *       x3[2] = 2; // end Array.const
 *       int32_t x6 = ((int)((x3).size()));
 *       for (int x7 = 0; x7 < x6; x7 += 1) {
 * 
 *         // begin Array.const
 *         std::vector<int32_t> x2(3);
 *         std::vector<int32_t> x3 = x2;
 *         x3[0] = 0;
 *         x3[1] = 1;
 *         x3[2] = 2; // end Array.const
 *         x5 = (x5 * (x3[x7]));
 *       }
 *       // end Array.fold
 *       return (x9 + x5);
 *     }
 *  |}];
 *   code |> Util.clang_build |> print_endline;
 *   [%expect
 *     {|
 *     <stdin>:79:24: error: redefinition of 'x2'
 *       std::vector<int32_t> x2 (3);  std::vector<int32_t> x3 = x2;  x3[0] = 0;  x3[1] = 1;  x3[2] = 2;// end Array.const
 *                            ^
 *     <stdin>:66:24: note: previous definition is here
 *       std::vector<int32_t> x2 (3);  std::vector<int32_t> x3 = x2;  x3[0] = 0;  x3[1] = 1;  x3[2] = 2;// end Array.const
 *                            ^
 *     <stdin>:79:54: error: redefinition of 'x3'
 *       std::vector<int32_t> x2 (3);  std::vector<int32_t> x3 = x2;  x3[0] = 0;  x3[1] = 1;  x3[2] = 2;// end Array.const
 *                                                          ^
 *     <stdin>:66:54: note: previous definition is here
 *       std::vector<int32_t> x2 (3);  std::vector<int32_t> x3 = x2;  x3[0] = 0;  x3[1] = 1;  x3[2] = 2;// end Array.const
 *                                                          ^
 *     2 errors generated. |}]
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let open Int in
 *   let arr =
 *     lazy
 *       (genlet @@ Array.const (Array.mk_type Int.type_) [| int 0; int 1; int 2 |])
 *   in
 *   let code =
 *     let_locus @@ fun () ->
 *     Array.fold (Lazy.force arr) ~init:(int 0) ~f:( + )
 *     + Array.fold (Lazy.force arr) ~init:(int 0) ~f:( * )
 *   in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int main() {
 *       // begin Array.const
 *       std::vector<int32_t> x2(3);
 *       std::vector<int32_t> x3 = x2;
 *       x3[0] = 0;
 *       x3[1] = 1;
 *       x3[2] = 2; // end Array.const
 *       std::vector<int32_t> x4 = x3;
 *       // begin Array.fold
 *       int32_t x9(0);
 *       int32_t x10 = x9;
 *       int32_t x11 = ((int)((x4).size()));
 *       for (int x12 = 0; x12 < x11; x12 += 1) {
 *         x10 = (x10 + (x4[x12]));
 *       }
 *       // end Array.fold
 * 
 *       // begin Array.fold
 *       int32_t x5(0);
 *       int32_t x6 = x5;
 *       int32_t x7 = ((int)((x4).size()));
 *       for (int x8 = 0; x8 < x7; x8 += 1) {
 *         x6 = (x6 * (x4[x8]));
 *       }
 *       // end Array.fold
 *       return (x10 + x6);
 *     }
 *  |}];
 *   code |> Util.clang_build |> print_endline;
 *   [%expect {| |}]
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module C = Cstage.Code (Core) in
 *   let open C in
 *   let code =
 *     let_ (Sexp.input ()) @@ fun sexp ->
 *     let_ (Array.of_sexp sexp String.of_sexp) @@ fun arr ->
 *     Array.iter arr ~f:(fun str -> String.print str)
 *   in
 *   let code = to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     int main() {
 *       sexp *x2 = sexp::load(std::cin);
 *       sexp *x3 = x2;
 *       std::vector<sexp *> x4 = ((list *\)(x3))->get_body();
 *       // begin Array.init
 *       std::vector<std::string> x5(((int)((x4).size())));
 *       std::vector<std::string> x6 = x5;
 *       for (int x7 = 0; x7 < ((int)((x4).size())); x7 += 1) {
 *         std::string x8 = ((atom *\)(((x4)[x7])))->get_body();
 *         x6[x7] = x8;
 *       }
 *       // end Array.init
 *       std::vector<std::string> x9 = x6;
 *       // begin Array.iter
 *       int32_t x10 = ((int)((x9).size()));
 *       for (int x11 = 0; x11 < x10; x11 += 1) {
 *         std::cout << (x9[x11]) << std::endl;
 *       }
 *       // end Array.iter
 *       return 0;
 *     } |}];
 *   let out = Util.clang_exec ~input:"(one two three)" code in
 *   out#exe_output |> print_endline;
 *   [%expect {|
 *     one
 *     two
 *     three |}]
 * 
 * let%expect_test "" =
 *   let module Core = Cstage_core.Make () in
 *   let module Array = Cstage_array.ArenaArray (Core) in
 *   let module C = Cstage.Code (Core) in
 *   let array_t = Array.mk_type C.Int.type_ in
 *   let code =
 *     C.let_ (Array.const array_t [| C.Int.int 1; C.Int.int 2; C.Int.int 3 |])
 *     @@ fun a1 ->
 *     C.let_ (Array.const array_t [| C.Int.int 1; C.Int.int 2; C.Int.int 3 |])
 *     @@ fun a2 ->
 *     C.ite
 *       Array.O.(a1 = a2)
 *       (fun () -> C.String.(print @@ const "true"))
 *       (fun () -> C.String.(print @@ const "false"))
 *   in
 *   let code = C.to_string code in
 *   code |> Util.clang_format |> print_endline;
 *   [%expect
 *     {|
 *     #include <array>
 *     #include <cmath>
 *     #include <iostream>
 *     #include <set>
 *     #include <unordered_set>
 *     #include <vector>
 * 
 *     #include "sexp.hpp"
 *     #include "stdlib.hpp"
 * 
 *     template <typename T> struct span {
 *       T *ptr;
 *       int len;
 * 
 *       bool operator==(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return false;
 *         } else {
 *           bool ret = true;
 *           for (int i = 0; i < len; i++) {
 *             ret = ret && (ptr[i] == rhs.ptr[i]);
 *           }
 *           return ret;
 *         }
 *       }
 * 
 *       bool operator<(const span<T> &rhs) const {
 *         if (len != rhs.len) {
 *           return len < rhs.len;
 *         }
 *         for (int i = 0; i < len; i++) {
 *           if (ptr[i] != rhs.ptr[i]) {
 *             return ptr[i] < rhs.ptr[i];
 *           }
 *         }
 *         return false;
 *       }
 * 
 *       bool operator<=(const span<T> &rhs) const {
 *         return this < rhs || this == rhs;
 *       }
 *       bool operator>(const span<T> &rhs) const { return !(this <= rhs); }
 *       bool operator>=(const span<T> &rhs) const { return !(this < rhs); }
 *     };
 * 
 *     namespace std {
 *     template <> struct hash<std::vector<int>> {
 *       size_t operator()(const vector<int> &v) const {
 *         std::hash<int> hasher;
 *         size_t seed = 0;
 *         for (int i : v) {
 *           seed ^= hasher(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 *         }
 *         return seed;
 *       }
 *     };
 *     } // namespace std
 * 
 *     int main();
 *     std::array<int32_t, 500000> x2;
 *     int32_t x3;
 *     int main() {
 *       // begin Array.const
 *       span<int32_t> x4 = (span<int32_t>){(x2).data() + x3, 3};
 *       x3 += 3;
 *       span<int32_t> x5 = x4;
 *       x5.ptr[0] = 1;
 *       x5.ptr[1] = 2;
 *       x5.ptr[2] = 3; // end Array.const
 *       span<int32_t> x6 = x5;
 *       // begin Array.const
 *       span<int32_t> x7 = (span<int32_t>){(x2).data() + x3, 3};
 *       x3 += 3;
 *       span<int32_t> x8 = x7;
 *       x8.ptr[0] = 1;
 *       x8.ptr[1] = 2;
 *       x8.ptr[2] = 3; // end Array.const
 *       span<int32_t> x9 = x8;
 *       int x10;
 *       int x11 = x10;
 *       if ((x6 == x9)) {
 *         std::cout << "true" << std::endl;
 *         x11 = 0;
 *       } else {
 *         std::cout << "false" << std::endl;
 *         x11 = 0;
 *       }
 *       return x11;
 *     } |}];
 *   let out = Util.clang_exec ~input:"(one two three)" code in
 *   out#exe_output |> print_endline;
 *   [%expect
 *     {|
 *     main.cpp:68:46: warning: variable 'x10' is uninitialized when used here [-Wuninitialized]
 *       span<int32_t> x9 = x8; int x10;  int x11 = x10;
 *                                                  ^~~
 *     main.cpp:68:33: note: initialize the variable 'x10' to silence this warning
 *       span<int32_t> x9 = x8; int x10;  int x11 = x10;
 *                                     ^
 *                                      = 0
 *     1 warning generated.
 *     true |}] *)
