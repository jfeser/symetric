open! Core

module Code = Cstage.Code ()

module Deepcoder = Deepcoder.Make (Code)

module Examples = struct
  open Code
  open Deepcoder

  let inputs =
    [
      ( "L",
        Value.A
          ( Array.const (Array.mk_type (Array.mk_type Int))
          @@ [|
               Array.const (Array.mk_type Int)
                 [| int 3; int 7; int 5; int 2; int 8 |];
             |] ) );
    ]

  let output =
    ( "L",
      Value.A
        ( Array.const (Array.mk_type (Array.mk_type Int))
        @@ [|
             Array.const (Array.mk_type Int)
               [| int 3; int 2; int 5; int 2; int 3 |];
           |] ) )
end

module DeepSynth =
  Synth.Make (Code) (Deepcoder.Lang (Examples)) (Deepcoder.Cache)

let%expect_test "" =
  DeepSynth.enumerate 1 |> Code.to_string |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int reconstruct_I(std::pair<std::vector<int>, std::vector<int>> &x370);
    int reconstruct_L(
        std::pair<std::vector<int>, std::vector<std::vector<int>>> &x14);
    int main();
    std::vector<int> x0(5);
    std::vector<std::vector<int>> x1(1);
    std::vector<int> x2(5);
    std::vector<std::vector<int>> x3(1);
    std::vector<std::set<std::pair<std::vector<int>, std::vector<int>>>> x4;
    std::set<std::pair<std::vector<int>, std::vector<int>>> x6;
    std::vector<
        std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>>>
        x7;
    std::set<std::pair<std::vector<std::vector<int>>, std::vector<int>>> x9;
    std::vector<int> x10(0);
    int x601;
    int main() {
      // begin Array.init
      x4.clear();
      x4.reserve(100);
      for (int x5 = 0; x5 < 100; x5++) {
        x4.push_back(x6);
      }
      // end Array.init

      // begin Array.init
      x7.clear();
      x7.reserve(100);
      for (int x8 = 0; x8 < 100; x8++) {
        x7.push_back(x9);
      }
      // end Array.init
      auto &x12 = (x7[1]);
      // begin Array.const
      // end Array.const

      // begin Array.const

      // begin Array.const
      x0[0] = 3;
      x0[1] = 7;
      x0[2] = 5;
      x0[3] = 2;
      x0[4] = 8;  // end Array.const
      x1[0] = x0; // end Array.const
      std::pair<std::vector<std::vector<int>>, std::vector<int>> x11;
      x11 = std::make_pair(x1, x10);
      x12.insert(x11);
      std::cout << "Starting reconstruction" << std::endl;
      // begin Array.const

      // begin Array.const
      x2[0] = 3;
      x2[1] = 2;
      x2[2] = 5;
      x2[3] = 2;
      x2[4] = 3;  // end Array.const
      x3[0] = x2; // end Array.const

      // begin Array.const
      // end Array.const
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x599;
      x599 = std::make_pair(x10, x3);
      int x600 = reconstruct_L(x599);
      exit(0);
      // begin Array.const

      // begin Array.const
      x2[0] = 3;
      x2[1] = 2;
      x2[2] = 5;
      x2[3] = 2;
      x2[4] = 3;  // end Array.const
      x3[0] = x2; // end Array.const

      // begin Array.const

      // begin Array.const
      x0[0] = 3;
      x0[1] = 7;
      x0[2] = 5;
      x0[3] = 2;
      x0[4] = 8;  // end Array.const
      x1[0] = x0; // end Array.const
      int x13 = (x1 == x3);
      if (x13) {
        x601 = 0;
      } else {
        x601 = 0;
      }
      return 0;
    }
    int reconstruct_L(
        std::pair<std::vector<int>, std::vector<std::vector<int>>> &x14) {
      int x18;
      std::vector<std::vector<int>> x25;
      std::vector<int> x29;
      int x36;
      std::vector<std::vector<int>> x42;
      std::vector<int> x46;
      int x53;
      std::vector<std::vector<int>> x59;
      std::vector<int> x63;
      int x70;
      std::vector<std::vector<int>> x76;
      std::vector<int> x80;
      int x87;
      std::vector<std::vector<int>> x93;
      std::vector<int> x97;
      int x104;
      std::vector<std::vector<int>> x110;
      std::vector<int> x114;
      int x121;
      std::vector<std::vector<int>> x127;
      std::vector<int> x131;
      int x138;
      std::vector<std::vector<int>> x144;
      std::vector<int> x148;
      int x155;
      std::vector<std::vector<int>> x161;
      std::vector<int> x165;
      int x172;
      std::vector<std::vector<int>> x178;
      std::vector<int> x182;
      int x189;
      std::vector<std::vector<int>> x195;
      std::vector<int> x199;
      int x207;
      std::vector<std::vector<int>> x219;
      std::vector<int> x226;
      int x231;
      int x237;
      std::vector<std::vector<int>> x247;
      std::vector<int> x254;
      int x259;
      int x265;
      std::vector<std::vector<int>> x275;
      std::vector<int> x282;
      int x292;
      std::vector<std::vector<int>> x302;
      std::vector<int> x309;
      int x319;
      std::vector<std::vector<int>> x329;
      std::vector<int> x336;
      int x346;
      std::vector<std::vector<int>> x356;
      std::vector<int> x364;
      int x564;
      std::vector<std::vector<int>> x574;
      std::vector<int> x580;
      int x589;
      int x592;
      int x595;
      int x598;
      auto &x15 = std::get<0>(x14);
      auto &x16 = std::get<1>(x14);
      auto &x209 = (x15[0]);
      auto &x208 = (x15[1]);
      auto &x210 = (x7[x208]);
      auto &x214 = (x7[x209]);
      std::cout << "(App zipwith ((Id max) (Id L23) (Id L24)))" << std::endl;
      auto &x217 = std::get<0>(*x215);
      auto &x216 = std::get<1>(*x215);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x232;
      x232 = std::make_pair(x216, x217);
      int x233 = reconstruct_L(x232);
      auto &x213 = std::get<0>(*x211);
      auto &x212 = std::get<1>(*x211);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x234;
      x234 = std::make_pair(x212, x213);
      int x235 = reconstruct_L(x234);
      // begin Array.init
      x219.clear();
      auto &x213 = std::get<0>(*x211);
      int x218 = (x213).size();
      x219.reserve(x218);
      auto &x213 = std::get<0>(*x211);
      int x218 = (x213).size();
      for (int x220 = 0; x220 < x218; x220++) {

        // begin Array.init
        x226.clear();
        auto &x217 = std::get<0>(*x215);
        auto &x221 = (x217[x220]);
        int x223 = (x221).size();
        auto &x213 = std::get<0>(*x211);
        auto &x222 = (x213[x220]);
        int x224 = (x222).size();
        int x225 = std::min(x224, x223);
        x226.reserve(x225);
        auto &x217 = std::get<0>(*x215);
        auto &x221 = (x217[x220]);
        int x223 = (x221).size();
        auto &x213 = std::get<0>(*x211);
        auto &x222 = (x213[x220]);
        int x224 = (x222).size();
        int x225 = std::min(x224, x223);
        for (int x227 = 0; x227 < x225; x227++) {
          auto &x217 = std::get<0>(*x215);
          auto &x221 = (x217[x220]);
          auto &x228 = (x221[x227]);
          auto &x213 = std::get<0>(*x211);
          auto &x222 = (x213[x220]);
          auto &x229 = (x222[x227]);
          auto &x217 = std::get<0>(*x215);
          auto &x221 = (x217[x220]);
          auto &x228 = (x221[x227]);
          auto &x213 = std::get<0>(*x211);
          auto &x222 = (x213[x220]);
          auto &x229 = (x222[x227]);
          int x230 = (x229 > x228);
          if (x230) {
            x231 = x229;
          } else {
            x231 = x228;
          }
          x226.push_back(x231);
        }
        // end Array.init
        x219.push_back(x226);
      }
      // end Array.init
      int x236 = (x219 == x16);
      if (x236) {
        x237 = 0;
      } else {
        x237 = 0;
      }
      for (auto x215 = x214.begin(); x215 != x214.end(); ++x215) {
        std::cout << "(App zipwith ((Id max) (Id L23) (Id L24)))" << std::endl;
        auto &x217 = std::get<0>(*x215);
        auto &x216 = std::get<1>(*x215);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x232;
        x232 = std::make_pair(x216, x217);
        int x233 = reconstruct_L(x232);
        auto &x213 = std::get<0>(*x211);
        auto &x212 = std::get<1>(*x211);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x234;
        x234 = std::make_pair(x212, x213);
        int x235 = reconstruct_L(x234);
        // begin Array.init
        x219.clear();
        auto &x213 = std::get<0>(*x211);
        int x218 = (x213).size();
        x219.reserve(x218);
        auto &x213 = std::get<0>(*x211);
        int x218 = (x213).size();
        for (int x220 = 0; x220 < x218; x220++) {

          // begin Array.init
          x226.clear();
          auto &x217 = std::get<0>(*x215);
          auto &x221 = (x217[x220]);
          int x223 = (x221).size();
          auto &x213 = std::get<0>(*x211);
          auto &x222 = (x213[x220]);
          int x224 = (x222).size();
          int x225 = std::min(x224, x223);
          x226.reserve(x225);
          auto &x217 = std::get<0>(*x215);
          auto &x221 = (x217[x220]);
          int x223 = (x221).size();
          auto &x213 = std::get<0>(*x211);
          auto &x222 = (x213[x220]);
          int x224 = (x222).size();
          int x225 = std::min(x224, x223);
          for (int x227 = 0; x227 < x225; x227++) {
            auto &x217 = std::get<0>(*x215);
            auto &x221 = (x217[x220]);
            auto &x228 = (x221[x227]);
            auto &x213 = std::get<0>(*x211);
            auto &x222 = (x213[x220]);
            auto &x229 = (x222[x227]);
            auto &x217 = std::get<0>(*x215);
            auto &x221 = (x217[x220]);
            auto &x228 = (x221[x227]);
            auto &x213 = std::get<0>(*x211);
            auto &x222 = (x213[x220]);
            auto &x229 = (x222[x227]);
            int x230 = (x229 > x228);
            if (x230) {
              x231 = x229;
            } else {
              x231 = x228;
            }
            x226.push_back(x231);
          }
          // end Array.init
          x219.push_back(x226);
        }
        // end Array.init
        int x236 = (x219 == x16);
        if (x236) {
          x237 = 0;
        } else {
          x237 = 0;
        }
      }
      for (auto x211 = x210.begin(); x211 != x210.end(); ++x211) {
        auto &x214 = (x7[x209]);
        std::cout << "(App zipwith ((Id max) (Id L23) (Id L24)))" << std::endl;
        auto &x217 = std::get<0>(*x215);
        auto &x216 = std::get<1>(*x215);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x232;
        x232 = std::make_pair(x216, x217);
        int x233 = reconstruct_L(x232);
        auto &x213 = std::get<0>(*x211);
        auto &x212 = std::get<1>(*x211);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x234;
        x234 = std::make_pair(x212, x213);
        int x235 = reconstruct_L(x234);
        // begin Array.init
        x219.clear();
        auto &x213 = std::get<0>(*x211);
        int x218 = (x213).size();
        x219.reserve(x218);
        auto &x213 = std::get<0>(*x211);
        int x218 = (x213).size();
        for (int x220 = 0; x220 < x218; x220++) {

          // begin Array.init
          x226.clear();
          auto &x217 = std::get<0>(*x215);
          auto &x221 = (x217[x220]);
          int x223 = (x221).size();
          auto &x213 = std::get<0>(*x211);
          auto &x222 = (x213[x220]);
          int x224 = (x222).size();
          int x225 = std::min(x224, x223);
          x226.reserve(x225);
          auto &x217 = std::get<0>(*x215);
          auto &x221 = (x217[x220]);
          int x223 = (x221).size();
          auto &x213 = std::get<0>(*x211);
          auto &x222 = (x213[x220]);
          int x224 = (x222).size();
          int x225 = std::min(x224, x223);
          for (int x227 = 0; x227 < x225; x227++) {
            auto &x217 = std::get<0>(*x215);
            auto &x221 = (x217[x220]);
            auto &x228 = (x221[x227]);
            auto &x213 = std::get<0>(*x211);
            auto &x222 = (x213[x220]);
            auto &x229 = (x222[x227]);
            auto &x217 = std::get<0>(*x215);
            auto &x221 = (x217[x220]);
            auto &x228 = (x221[x227]);
            auto &x213 = std::get<0>(*x211);
            auto &x222 = (x213[x220]);
            auto &x229 = (x222[x227]);
            int x230 = (x229 > x228);
            if (x230) {
              x231 = x229;
            } else {
              x231 = x228;
            }
            x226.push_back(x231);
          }
          // end Array.init
          x219.push_back(x226);
        }
        // end Array.init
        int x236 = (x219 == x16);
        if (x236) {
          x237 = 0;
        } else {
          x237 = 0;
        }
        for (auto x215 = x214.begin(); x215 != x214.end(); ++x215) {
          std::cout << "(App zipwith ((Id max) (Id L23) (Id L24)))" << std::endl;
          auto &x217 = std::get<0>(*x215);
          auto &x216 = std::get<1>(*x215);
          std::pair<std::vector<int>, std::vector<std::vector<int>>> x232;
          x232 = std::make_pair(x216, x217);
          int x233 = reconstruct_L(x232);
          auto &x213 = std::get<0>(*x211);
          auto &x212 = std::get<1>(*x211);
          std::pair<std::vector<int>, std::vector<std::vector<int>>> x234;
          x234 = std::make_pair(x212, x213);
          int x235 = reconstruct_L(x234);
          // begin Array.init
          x219.clear();
          auto &x213 = std::get<0>(*x211);
          int x218 = (x213).size();
          x219.reserve(x218);
          auto &x213 = std::get<0>(*x211);
          int x218 = (x213).size();
          for (int x220 = 0; x220 < x218; x220++) {

            // begin Array.init
            x226.clear();
            auto &x217 = std::get<0>(*x215);
            auto &x221 = (x217[x220]);
            int x223 = (x221).size();
            auto &x213 = std::get<0>(*x211);
            auto &x222 = (x213[x220]);
            int x224 = (x222).size();
            int x225 = std::min(x224, x223);
            x226.reserve(x225);
            auto &x217 = std::get<0>(*x215);
            auto &x221 = (x217[x220]);
            int x223 = (x221).size();
            auto &x213 = std::get<0>(*x211);
            auto &x222 = (x213[x220]);
            int x224 = (x222).size();
            int x225 = std::min(x224, x223);
            for (int x227 = 0; x227 < x225; x227++) {
              auto &x217 = std::get<0>(*x215);
              auto &x221 = (x217[x220]);
              auto &x228 = (x221[x227]);
              auto &x213 = std::get<0>(*x211);
              auto &x222 = (x213[x220]);
              auto &x229 = (x222[x227]);
              auto &x217 = std::get<0>(*x215);
              auto &x221 = (x217[x220]);
              auto &x228 = (x221[x227]);
              auto &x213 = std::get<0>(*x211);
              auto &x222 = (x213[x220]);
              auto &x229 = (x222[x227]);
              int x230 = (x229 > x228);
              if (x230) {
                x231 = x229;
              } else {
                x231 = x228;
              }
              x226.push_back(x231);
            }
            // end Array.init
            x219.push_back(x226);
          }
          // end Array.init
          int x236 = (x219 == x16);
          if (x236) {
            x237 = 0;
          } else {
            x237 = 0;
          }
        }
      }
      auto &x238 = (x7[x208]);
      auto &x242 = (x7[x209]);
      std::cout << "(App zipwith ((Id min) (Id L21) (Id L22)))" << std::endl;
      auto &x245 = std::get<0>(*x243);
      auto &x244 = std::get<1>(*x243);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x260;
      x260 = std::make_pair(x244, x245);
      int x261 = reconstruct_L(x260);
      auto &x241 = std::get<0>(*x239);
      auto &x240 = std::get<1>(*x239);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x262;
      x262 = std::make_pair(x240, x241);
      int x263 = reconstruct_L(x262);
      // begin Array.init
      x247.clear();
      auto &x241 = std::get<0>(*x239);
      int x246 = (x241).size();
      x247.reserve(x246);
      auto &x241 = std::get<0>(*x239);
      int x246 = (x241).size();
      for (int x248 = 0; x248 < x246; x248++) {

        // begin Array.init
        x254.clear();
        auto &x245 = std::get<0>(*x243);
        auto &x249 = (x245[x248]);
        int x251 = (x249).size();
        auto &x241 = std::get<0>(*x239);
        auto &x250 = (x241[x248]);
        int x252 = (x250).size();
        int x253 = std::min(x252, x251);
        x254.reserve(x253);
        auto &x245 = std::get<0>(*x243);
        auto &x249 = (x245[x248]);
        int x251 = (x249).size();
        auto &x241 = std::get<0>(*x239);
        auto &x250 = (x241[x248]);
        int x252 = (x250).size();
        int x253 = std::min(x252, x251);
        for (int x255 = 0; x255 < x253; x255++) {
          auto &x245 = std::get<0>(*x243);
          auto &x249 = (x245[x248]);
          auto &x256 = (x249[x255]);
          auto &x241 = std::get<0>(*x239);
          auto &x250 = (x241[x248]);
          auto &x257 = (x250[x255]);
          auto &x245 = std::get<0>(*x243);
          auto &x249 = (x245[x248]);
          auto &x256 = (x249[x255]);
          auto &x241 = std::get<0>(*x239);
          auto &x250 = (x241[x248]);
          auto &x257 = (x250[x255]);
          int x258 = (x257 < x256);
          if (x258) {
            x259 = x257;
          } else {
            x259 = x256;
          }
          x254.push_back(x259);
        }
        // end Array.init
        x247.push_back(x254);
      }
      // end Array.init
      int x264 = (x247 == x16);
      if (x264) {
        x265 = 0;
      } else {
        x265 = 0;
      }
      for (auto x243 = x242.begin(); x243 != x242.end(); ++x243) {
        std::cout << "(App zipwith ((Id min) (Id L21) (Id L22)))" << std::endl;
        auto &x245 = std::get<0>(*x243);
        auto &x244 = std::get<1>(*x243);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x260;
        x260 = std::make_pair(x244, x245);
        int x261 = reconstruct_L(x260);
        auto &x241 = std::get<0>(*x239);
        auto &x240 = std::get<1>(*x239);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x262;
        x262 = std::make_pair(x240, x241);
        int x263 = reconstruct_L(x262);
        // begin Array.init
        x247.clear();
        auto &x241 = std::get<0>(*x239);
        int x246 = (x241).size();
        x247.reserve(x246);
        auto &x241 = std::get<0>(*x239);
        int x246 = (x241).size();
        for (int x248 = 0; x248 < x246; x248++) {

          // begin Array.init
          x254.clear();
          auto &x245 = std::get<0>(*x243);
          auto &x249 = (x245[x248]);
          int x251 = (x249).size();
          auto &x241 = std::get<0>(*x239);
          auto &x250 = (x241[x248]);
          int x252 = (x250).size();
          int x253 = std::min(x252, x251);
          x254.reserve(x253);
          auto &x245 = std::get<0>(*x243);
          auto &x249 = (x245[x248]);
          int x251 = (x249).size();
          auto &x241 = std::get<0>(*x239);
          auto &x250 = (x241[x248]);
          int x252 = (x250).size();
          int x253 = std::min(x252, x251);
          for (int x255 = 0; x255 < x253; x255++) {
            auto &x245 = std::get<0>(*x243);
            auto &x249 = (x245[x248]);
            auto &x256 = (x249[x255]);
            auto &x241 = std::get<0>(*x239);
            auto &x250 = (x241[x248]);
            auto &x257 = (x250[x255]);
            auto &x245 = std::get<0>(*x243);
            auto &x249 = (x245[x248]);
            auto &x256 = (x249[x255]);
            auto &x241 = std::get<0>(*x239);
            auto &x250 = (x241[x248]);
            auto &x257 = (x250[x255]);
            int x258 = (x257 < x256);
            if (x258) {
              x259 = x257;
            } else {
              x259 = x256;
            }
            x254.push_back(x259);
          }
          // end Array.init
          x247.push_back(x254);
        }
        // end Array.init
        int x264 = (x247 == x16);
        if (x264) {
          x265 = 0;
        } else {
          x265 = 0;
        }
      }
      for (auto x239 = x238.begin(); x239 != x238.end(); ++x239) {
        auto &x242 = (x7[x209]);
        std::cout << "(App zipwith ((Id min) (Id L21) (Id L22)))" << std::endl;
        auto &x245 = std::get<0>(*x243);
        auto &x244 = std::get<1>(*x243);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x260;
        x260 = std::make_pair(x244, x245);
        int x261 = reconstruct_L(x260);
        auto &x241 = std::get<0>(*x239);
        auto &x240 = std::get<1>(*x239);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x262;
        x262 = std::make_pair(x240, x241);
        int x263 = reconstruct_L(x262);
        // begin Array.init
        x247.clear();
        auto &x241 = std::get<0>(*x239);
        int x246 = (x241).size();
        x247.reserve(x246);
        auto &x241 = std::get<0>(*x239);
        int x246 = (x241).size();
        for (int x248 = 0; x248 < x246; x248++) {

          // begin Array.init
          x254.clear();
          auto &x245 = std::get<0>(*x243);
          auto &x249 = (x245[x248]);
          int x251 = (x249).size();
          auto &x241 = std::get<0>(*x239);
          auto &x250 = (x241[x248]);
          int x252 = (x250).size();
          int x253 = std::min(x252, x251);
          x254.reserve(x253);
          auto &x245 = std::get<0>(*x243);
          auto &x249 = (x245[x248]);
          int x251 = (x249).size();
          auto &x241 = std::get<0>(*x239);
          auto &x250 = (x241[x248]);
          int x252 = (x250).size();
          int x253 = std::min(x252, x251);
          for (int x255 = 0; x255 < x253; x255++) {
            auto &x245 = std::get<0>(*x243);
            auto &x249 = (x245[x248]);
            auto &x256 = (x249[x255]);
            auto &x241 = std::get<0>(*x239);
            auto &x250 = (x241[x248]);
            auto &x257 = (x250[x255]);
            auto &x245 = std::get<0>(*x243);
            auto &x249 = (x245[x248]);
            auto &x256 = (x249[x255]);
            auto &x241 = std::get<0>(*x239);
            auto &x250 = (x241[x248]);
            auto &x257 = (x250[x255]);
            int x258 = (x257 < x256);
            if (x258) {
              x259 = x257;
            } else {
              x259 = x256;
            }
            x254.push_back(x259);
          }
          // end Array.init
          x247.push_back(x254);
        }
        // end Array.init
        int x264 = (x247 == x16);
        if (x264) {
          x265 = 0;
        } else {
          x265 = 0;
        }
        for (auto x243 = x242.begin(); x243 != x242.end(); ++x243) {
          std::cout << "(App zipwith ((Id min) (Id L21) (Id L22)))" << std::endl;
          auto &x245 = std::get<0>(*x243);
          auto &x244 = std::get<1>(*x243);
          std::pair<std::vector<int>, std::vector<std::vector<int>>> x260;
          x260 = std::make_pair(x244, x245);
          int x261 = reconstruct_L(x260);
          auto &x241 = std::get<0>(*x239);
          auto &x240 = std::get<1>(*x239);
          std::pair<std::vector<int>, std::vector<std::vector<int>>> x262;
          x262 = std::make_pair(x240, x241);
          int x263 = reconstruct_L(x262);
          // begin Array.init
          x247.clear();
          auto &x241 = std::get<0>(*x239);
          int x246 = (x241).size();
          x247.reserve(x246);
          auto &x241 = std::get<0>(*x239);
          int x246 = (x241).size();
          for (int x248 = 0; x248 < x246; x248++) {

            // begin Array.init
            x254.clear();
            auto &x245 = std::get<0>(*x243);
            auto &x249 = (x245[x248]);
            int x251 = (x249).size();
            auto &x241 = std::get<0>(*x239);
            auto &x250 = (x241[x248]);
            int x252 = (x250).size();
            int x253 = std::min(x252, x251);
            x254.reserve(x253);
            auto &x245 = std::get<0>(*x243);
            auto &x249 = (x245[x248]);
            int x251 = (x249).size();
            auto &x241 = std::get<0>(*x239);
            auto &x250 = (x241[x248]);
            int x252 = (x250).size();
            int x253 = std::min(x252, x251);
            for (int x255 = 0; x255 < x253; x255++) {
              auto &x245 = std::get<0>(*x243);
              auto &x249 = (x245[x248]);
              auto &x256 = (x249[x255]);
              auto &x241 = std::get<0>(*x239);
              auto &x250 = (x241[x248]);
              auto &x257 = (x250[x255]);
              auto &x245 = std::get<0>(*x243);
              auto &x249 = (x245[x248]);
              auto &x256 = (x249[x255]);
              auto &x241 = std::get<0>(*x239);
              auto &x250 = (x241[x248]);
              auto &x257 = (x250[x255]);
              int x258 = (x257 < x256);
              if (x258) {
                x259 = x257;
              } else {
                x259 = x256;
              }
              x254.push_back(x259);
            }
            // end Array.init
            x247.push_back(x254);
          }
          // end Array.init
          int x264 = (x247 == x16);
          if (x264) {
            x265 = 0;
          } else {
            x265 = 0;
          }
        }
      }
      auto &x266 = (x7[x208]);
      auto &x270 = (x7[x209]);
      std::cout << "(App zipwith ((Id \"(*)\") (Id L19) (Id L20)))" << std::endl;
      auto &x273 = std::get<0>(*x271);
      auto &x272 = std::get<1>(*x271);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x287;
      x287 = std::make_pair(x272, x273);
      int x288 = reconstruct_L(x287);
      auto &x269 = std::get<0>(*x267);
      auto &x268 = std::get<1>(*x267);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x289;
      x289 = std::make_pair(x268, x269);
      int x290 = reconstruct_L(x289);
      // begin Array.init
      x275.clear();
      auto &x269 = std::get<0>(*x267);
      int x274 = (x269).size();
      x275.reserve(x274);
      auto &x269 = std::get<0>(*x267);
      int x274 = (x269).size();
      for (int x276 = 0; x276 < x274; x276++) {

        // begin Array.init
        x282.clear();
        auto &x273 = std::get<0>(*x271);
        auto &x277 = (x273[x276]);
        int x279 = (x277).size();
        auto &x269 = std::get<0>(*x267);
        auto &x278 = (x269[x276]);
        int x280 = (x278).size();
        int x281 = std::min(x280, x279);
        x282.reserve(x281);
        auto &x273 = std::get<0>(*x271);
        auto &x277 = (x273[x276]);
        int x279 = (x277).size();
        auto &x269 = std::get<0>(*x267);
        auto &x278 = (x269[x276]);
        int x280 = (x278).size();
        int x281 = std::min(x280, x279);
        for (int x283 = 0; x283 < x281; x283++) {
          auto &x273 = std::get<0>(*x271);
          auto &x277 = (x273[x276]);
          auto &x284 = (x277[x283]);
          auto &x269 = std::get<0>(*x267);
          auto &x278 = (x269[x276]);
          auto &x285 = (x278[x283]);
          int x286 = (x285 * x284);
          x282.push_back(x286);
        }
        // end Array.init
        x275.push_back(x282);
      }
      // end Array.init
      int x291 = (x275 == x16);
      if (x291) {
        x292 = 0;
      } else {
        x292 = 0;
      }
      for (auto x271 = x270.begin(); x271 != x270.end(); ++x271) {
        std::cout << "(App zipwith ((Id \"(*)\") (Id L19) (Id L20)))" << std::endl;
        auto &x273 = std::get<0>(*x271);
        auto &x272 = std::get<1>(*x271);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x287;
        x287 = std::make_pair(x272, x273);
        int x288 = reconstruct_L(x287);
        auto &x269 = std::get<0>(*x267);
        auto &x268 = std::get<1>(*x267);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x289;
        x289 = std::make_pair(x268, x269);
        int x290 = reconstruct_L(x289);
        // begin Array.init
        x275.clear();
        auto &x269 = std::get<0>(*x267);
        int x274 = (x269).size();
        x275.reserve(x274);
        auto &x269 = std::get<0>(*x267);
        int x274 = (x269).size();
        for (int x276 = 0; x276 < x274; x276++) {

          // begin Array.init
          x282.clear();
          auto &x273 = std::get<0>(*x271);
          auto &x277 = (x273[x276]);
          int x279 = (x277).size();
          auto &x269 = std::get<0>(*x267);
          auto &x278 = (x269[x276]);
          int x280 = (x278).size();
          int x281 = std::min(x280, x279);
          x282.reserve(x281);
          auto &x273 = std::get<0>(*x271);
          auto &x277 = (x273[x276]);
          int x279 = (x277).size();
          auto &x269 = std::get<0>(*x267);
          auto &x278 = (x269[x276]);
          int x280 = (x278).size();
          int x281 = std::min(x280, x279);
          for (int x283 = 0; x283 < x281; x283++) {
            auto &x273 = std::get<0>(*x271);
            auto &x277 = (x273[x276]);
            auto &x284 = (x277[x283]);
            auto &x269 = std::get<0>(*x267);
            auto &x278 = (x269[x276]);
            auto &x285 = (x278[x283]);
            int x286 = (x285 * x284);
            x282.push_back(x286);
          }
          // end Array.init
          x275.push_back(x282);
        }
        // end Array.init
        int x291 = (x275 == x16);
        if (x291) {
          x292 = 0;
        } else {
          x292 = 0;
        }
      }
      for (auto x267 = x266.begin(); x267 != x266.end(); ++x267) {
        auto &x270 = (x7[x209]);
        std::cout << "(App zipwith ((Id \"(*)\") (Id L19) (Id L20)))" << std::endl;
        auto &x273 = std::get<0>(*x271);
        auto &x272 = std::get<1>(*x271);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x287;
        x287 = std::make_pair(x272, x273);
        int x288 = reconstruct_L(x287);
        auto &x269 = std::get<0>(*x267);
        auto &x268 = std::get<1>(*x267);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x289;
        x289 = std::make_pair(x268, x269);
        int x290 = reconstruct_L(x289);
        // begin Array.init
        x275.clear();
        auto &x269 = std::get<0>(*x267);
        int x274 = (x269).size();
        x275.reserve(x274);
        auto &x269 = std::get<0>(*x267);
        int x274 = (x269).size();
        for (int x276 = 0; x276 < x274; x276++) {

          // begin Array.init
          x282.clear();
          auto &x273 = std::get<0>(*x271);
          auto &x277 = (x273[x276]);
          int x279 = (x277).size();
          auto &x269 = std::get<0>(*x267);
          auto &x278 = (x269[x276]);
          int x280 = (x278).size();
          int x281 = std::min(x280, x279);
          x282.reserve(x281);
          auto &x273 = std::get<0>(*x271);
          auto &x277 = (x273[x276]);
          int x279 = (x277).size();
          auto &x269 = std::get<0>(*x267);
          auto &x278 = (x269[x276]);
          int x280 = (x278).size();
          int x281 = std::min(x280, x279);
          for (int x283 = 0; x283 < x281; x283++) {
            auto &x273 = std::get<0>(*x271);
            auto &x277 = (x273[x276]);
            auto &x284 = (x277[x283]);
            auto &x269 = std::get<0>(*x267);
            auto &x278 = (x269[x276]);
            auto &x285 = (x278[x283]);
            int x286 = (x285 * x284);
            x282.push_back(x286);
          }
          // end Array.init
          x275.push_back(x282);
        }
        // end Array.init
        int x291 = (x275 == x16);
        if (x291) {
          x292 = 0;
        } else {
          x292 = 0;
        }
        for (auto x271 = x270.begin(); x271 != x270.end(); ++x271) {
          std::cout << "(App zipwith ((Id \"(*)\") (Id L19) (Id L20)))"
                    << std::endl;
          auto &x273 = std::get<0>(*x271);
          auto &x272 = std::get<1>(*x271);
          std::pair<std::vector<int>, std::vector<std::vector<int>>> x287;
          x287 = std::make_pair(x272, x273);
          int x288 = reconstruct_L(x287);
          auto &x269 = std::get<0>(*x267);
          auto &x268 = std::get<1>(*x267);
          std::pair<std::vector<int>, std::vector<std::vector<int>>> x289;
          x289 = std::make_pair(x268, x269);
          int x290 = reconstruct_L(x289);
          // begin Array.init
          x275.clear();
          auto &x269 = std::get<0>(*x267);
          int x274 = (x269).size();
          x275.reserve(x274);
          auto &x269 = std::get<0>(*x267);
          int x274 = (x269).size();
          for (int x276 = 0; x276 < x274; x276++) {

            // begin Array.init
            x282.clear();
            auto &x273 = std::get<0>(*x271);
            auto &x277 = (x273[x276]);
            int x279 = (x277).size();
            auto &x269 = std::get<0>(*x267);
            auto &x278 = (x269[x276]);
            int x280 = (x278).size();
            int x281 = std::min(x280, x279);
            x282.reserve(x281);
            auto &x273 = std::get<0>(*x271);
            auto &x277 = (x273[x276]);
            int x279 = (x277).size();
            auto &x269 = std::get<0>(*x267);
            auto &x278 = (x269[x276]);
            int x280 = (x278).size();
            int x281 = std::min(x280, x279);
            for (int x283 = 0; x283 < x281; x283++) {
              auto &x273 = std::get<0>(*x271);
              auto &x277 = (x273[x276]);
              auto &x284 = (x277[x283]);
              auto &x269 = std::get<0>(*x267);
              auto &x278 = (x269[x276]);
              auto &x285 = (x278[x283]);
              int x286 = (x285 * x284);
              x282.push_back(x286);
            }
            // end Array.init
            x275.push_back(x282);
          }
          // end Array.init
          int x291 = (x275 == x16);
          if (x291) {
            x292 = 0;
          } else {
            x292 = 0;
          }
        }
      }
      auto &x293 = (x7[x208]);
      auto &x297 = (x7[x209]);
      std::cout << "(App zipwith ((Id \"(-)\") (Id L17) (Id L18)))" << std::endl;
      auto &x300 = std::get<0>(*x298);
      auto &x299 = std::get<1>(*x298);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x314;
      x314 = std::make_pair(x299, x300);
      int x315 = reconstruct_L(x314);
      auto &x296 = std::get<0>(*x294);
      auto &x295 = std::get<1>(*x294);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x316;
      x316 = std::make_pair(x295, x296);
      int x317 = reconstruct_L(x316);
      // begin Array.init
      x302.clear();
      auto &x296 = std::get<0>(*x294);
      int x301 = (x296).size();
      x302.reserve(x301);
      auto &x296 = std::get<0>(*x294);
      int x301 = (x296).size();
      for (int x303 = 0; x303 < x301; x303++) {

        // begin Array.init
        x309.clear();
        auto &x300 = std::get<0>(*x298);
        auto &x304 = (x300[x303]);
        int x306 = (x304).size();
        auto &x296 = std::get<0>(*x294);
        auto &x305 = (x296[x303]);
        int x307 = (x305).size();
        int x308 = std::min(x307, x306);
        x309.reserve(x308);
        auto &x300 = std::get<0>(*x298);
        auto &x304 = (x300[x303]);
        int x306 = (x304).size();
        auto &x296 = std::get<0>(*x294);
        auto &x305 = (x296[x303]);
        int x307 = (x305).size();
        int x308 = std::min(x307, x306);
        for (int x310 = 0; x310 < x308; x310++) {
          auto &x300 = std::get<0>(*x298);
          auto &x304 = (x300[x303]);
          auto &x311 = (x304[x310]);
          auto &x296 = std::get<0>(*x294);
          auto &x305 = (x296[x303]);
          auto &x312 = (x305[x310]);
          int x313 = (x312 - x311);
          x309.push_back(x313);
        }
        // end Array.init
        x302.push_back(x309);
      }
      // end Array.init
      int x318 = (x302 == x16);
      if (x318) {
        x319 = 0;
      } else {
        x319 = 0;
      }
      for (auto x298 = x297.begin(); x298 != x297.end(); ++x298) {
        std::cout << "(App zipwith ((Id \"(-)\") (Id L17) (Id L18)))" << std::endl;
        auto &x300 = std::get<0>(*x298);
        auto &x299 = std::get<1>(*x298);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x314;
        x314 = std::make_pair(x299, x300);
        int x315 = reconstruct_L(x314);
        auto &x296 = std::get<0>(*x294);
        auto &x295 = std::get<1>(*x294);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x316;
        x316 = std::make_pair(x295, x296);
        int x317 = reconstruct_L(x316);
        // begin Array.init
        x302.clear();
        auto &x296 = std::get<0>(*x294);
        int x301 = (x296).size();
        x302.reserve(x301);
        auto &x296 = std::get<0>(*x294);
        int x301 = (x296).size();
        for (int x303 = 0; x303 < x301; x303++) {

          // begin Array.init
          x309.clear();
          auto &x300 = std::get<0>(*x298);
          auto &x304 = (x300[x303]);
          int x306 = (x304).size();
          auto &x296 = std::get<0>(*x294);
          auto &x305 = (x296[x303]);
          int x307 = (x305).size();
          int x308 = std::min(x307, x306);
          x309.reserve(x308);
          auto &x300 = std::get<0>(*x298);
          auto &x304 = (x300[x303]);
          int x306 = (x304).size();
          auto &x296 = std::get<0>(*x294);
          auto &x305 = (x296[x303]);
          int x307 = (x305).size();
          int x308 = std::min(x307, x306);
          for (int x310 = 0; x310 < x308; x310++) {
            auto &x300 = std::get<0>(*x298);
            auto &x304 = (x300[x303]);
            auto &x311 = (x304[x310]);
            auto &x296 = std::get<0>(*x294);
            auto &x305 = (x296[x303]);
            auto &x312 = (x305[x310]);
            int x313 = (x312 - x311);
            x309.push_back(x313);
          }
          // end Array.init
          x302.push_back(x309);
        }
        // end Array.init
        int x318 = (x302 == x16);
        if (x318) {
          x319 = 0;
        } else {
          x319 = 0;
        }
      }
      for (auto x294 = x293.begin(); x294 != x293.end(); ++x294) {
        auto &x297 = (x7[x209]);
        std::cout << "(App zipwith ((Id \"(-)\") (Id L17) (Id L18)))" << std::endl;
        auto &x300 = std::get<0>(*x298);
        auto &x299 = std::get<1>(*x298);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x314;
        x314 = std::make_pair(x299, x300);
        int x315 = reconstruct_L(x314);
        auto &x296 = std::get<0>(*x294);
        auto &x295 = std::get<1>(*x294);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x316;
        x316 = std::make_pair(x295, x296);
        int x317 = reconstruct_L(x316);
        // begin Array.init
        x302.clear();
        auto &x296 = std::get<0>(*x294);
        int x301 = (x296).size();
        x302.reserve(x301);
        auto &x296 = std::get<0>(*x294);
        int x301 = (x296).size();
        for (int x303 = 0; x303 < x301; x303++) {

          // begin Array.init
          x309.clear();
          auto &x300 = std::get<0>(*x298);
          auto &x304 = (x300[x303]);
          int x306 = (x304).size();
          auto &x296 = std::get<0>(*x294);
          auto &x305 = (x296[x303]);
          int x307 = (x305).size();
          int x308 = std::min(x307, x306);
          x309.reserve(x308);
          auto &x300 = std::get<0>(*x298);
          auto &x304 = (x300[x303]);
          int x306 = (x304).size();
          auto &x296 = std::get<0>(*x294);
          auto &x305 = (x296[x303]);
          int x307 = (x305).size();
          int x308 = std::min(x307, x306);
          for (int x310 = 0; x310 < x308; x310++) {
            auto &x300 = std::get<0>(*x298);
            auto &x304 = (x300[x303]);
            auto &x311 = (x304[x310]);
            auto &x296 = std::get<0>(*x294);
            auto &x305 = (x296[x303]);
            auto &x312 = (x305[x310]);
            int x313 = (x312 - x311);
            x309.push_back(x313);
          }
          // end Array.init
          x302.push_back(x309);
        }
        // end Array.init
        int x318 = (x302 == x16);
        if (x318) {
          x319 = 0;
        } else {
          x319 = 0;
        }
        for (auto x298 = x297.begin(); x298 != x297.end(); ++x298) {
          std::cout << "(App zipwith ((Id \"(-)\") (Id L17) (Id L18)))"
                    << std::endl;
          auto &x300 = std::get<0>(*x298);
          auto &x299 = std::get<1>(*x298);
          std::pair<std::vector<int>, std::vector<std::vector<int>>> x314;
          x314 = std::make_pair(x299, x300);
          int x315 = reconstruct_L(x314);
          auto &x296 = std::get<0>(*x294);
          auto &x295 = std::get<1>(*x294);
          std::pair<std::vector<int>, std::vector<std::vector<int>>> x316;
          x316 = std::make_pair(x295, x296);
          int x317 = reconstruct_L(x316);
          // begin Array.init
          x302.clear();
          auto &x296 = std::get<0>(*x294);
          int x301 = (x296).size();
          x302.reserve(x301);
          auto &x296 = std::get<0>(*x294);
          int x301 = (x296).size();
          for (int x303 = 0; x303 < x301; x303++) {

            // begin Array.init
            x309.clear();
            auto &x300 = std::get<0>(*x298);
            auto &x304 = (x300[x303]);
            int x306 = (x304).size();
            auto &x296 = std::get<0>(*x294);
            auto &x305 = (x296[x303]);
            int x307 = (x305).size();
            int x308 = std::min(x307, x306);
            x309.reserve(x308);
            auto &x300 = std::get<0>(*x298);
            auto &x304 = (x300[x303]);
            int x306 = (x304).size();
            auto &x296 = std::get<0>(*x294);
            auto &x305 = (x296[x303]);
            int x307 = (x305).size();
            int x308 = std::min(x307, x306);
            for (int x310 = 0; x310 < x308; x310++) {
              auto &x300 = std::get<0>(*x298);
              auto &x304 = (x300[x303]);
              auto &x311 = (x304[x310]);
              auto &x296 = std::get<0>(*x294);
              auto &x305 = (x296[x303]);
              auto &x312 = (x305[x310]);
              int x313 = (x312 - x311);
              x309.push_back(x313);
            }
            // end Array.init
            x302.push_back(x309);
          }
          // end Array.init
          int x318 = (x302 == x16);
          if (x318) {
            x319 = 0;
          } else {
            x319 = 0;
          }
        }
      }
      auto &x320 = (x7[x208]);
      auto &x324 = (x7[x209]);
      std::cout << "(App zipwith ((Id \"(+)\") (Id L15) (Id L16)))" << std::endl;
      auto &x327 = std::get<0>(*x325);
      auto &x326 = std::get<1>(*x325);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x341;
      x341 = std::make_pair(x326, x327);
      int x342 = reconstruct_L(x341);
      auto &x323 = std::get<0>(*x321);
      auto &x322 = std::get<1>(*x321);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x343;
      x343 = std::make_pair(x322, x323);
      int x344 = reconstruct_L(x343);
      // begin Array.init
      x329.clear();
      auto &x323 = std::get<0>(*x321);
      int x328 = (x323).size();
      x329.reserve(x328);
      auto &x323 = std::get<0>(*x321);
      int x328 = (x323).size();
      for (int x330 = 0; x330 < x328; x330++) {

        // begin Array.init
        x336.clear();
        auto &x327 = std::get<0>(*x325);
        auto &x331 = (x327[x330]);
        int x333 = (x331).size();
        auto &x323 = std::get<0>(*x321);
        auto &x332 = (x323[x330]);
        int x334 = (x332).size();
        int x335 = std::min(x334, x333);
        x336.reserve(x335);
        auto &x327 = std::get<0>(*x325);
        auto &x331 = (x327[x330]);
        int x333 = (x331).size();
        auto &x323 = std::get<0>(*x321);
        auto &x332 = (x323[x330]);
        int x334 = (x332).size();
        int x335 = std::min(x334, x333);
        for (int x337 = 0; x337 < x335; x337++) {
          auto &x327 = std::get<0>(*x325);
          auto &x331 = (x327[x330]);
          auto &x338 = (x331[x337]);
          auto &x323 = std::get<0>(*x321);
          auto &x332 = (x323[x330]);
          auto &x339 = (x332[x337]);
          int x340 = (x339 + x338);
          x336.push_back(x340);
        }
        // end Array.init
        x329.push_back(x336);
      }
      // end Array.init
      int x345 = (x329 == x16);
      if (x345) {
        x346 = 0;
      } else {
        x346 = 0;
      }
      for (auto x325 = x324.begin(); x325 != x324.end(); ++x325) {
        std::cout << "(App zipwith ((Id \"(+)\") (Id L15) (Id L16)))" << std::endl;
        auto &x327 = std::get<0>(*x325);
        auto &x326 = std::get<1>(*x325);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x341;
        x341 = std::make_pair(x326, x327);
        int x342 = reconstruct_L(x341);
        auto &x323 = std::get<0>(*x321);
        auto &x322 = std::get<1>(*x321);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x343;
        x343 = std::make_pair(x322, x323);
        int x344 = reconstruct_L(x343);
        // begin Array.init
        x329.clear();
        auto &x323 = std::get<0>(*x321);
        int x328 = (x323).size();
        x329.reserve(x328);
        auto &x323 = std::get<0>(*x321);
        int x328 = (x323).size();
        for (int x330 = 0; x330 < x328; x330++) {

          // begin Array.init
          x336.clear();
          auto &x327 = std::get<0>(*x325);
          auto &x331 = (x327[x330]);
          int x333 = (x331).size();
          auto &x323 = std::get<0>(*x321);
          auto &x332 = (x323[x330]);
          int x334 = (x332).size();
          int x335 = std::min(x334, x333);
          x336.reserve(x335);
          auto &x327 = std::get<0>(*x325);
          auto &x331 = (x327[x330]);
          int x333 = (x331).size();
          auto &x323 = std::get<0>(*x321);
          auto &x332 = (x323[x330]);
          int x334 = (x332).size();
          int x335 = std::min(x334, x333);
          for (int x337 = 0; x337 < x335; x337++) {
            auto &x327 = std::get<0>(*x325);
            auto &x331 = (x327[x330]);
            auto &x338 = (x331[x337]);
            auto &x323 = std::get<0>(*x321);
            auto &x332 = (x323[x330]);
            auto &x339 = (x332[x337]);
            int x340 = (x339 + x338);
            x336.push_back(x340);
          }
          // end Array.init
          x329.push_back(x336);
        }
        // end Array.init
        int x345 = (x329 == x16);
        if (x345) {
          x346 = 0;
        } else {
          x346 = 0;
        }
      }
      for (auto x321 = x320.begin(); x321 != x320.end(); ++x321) {
        auto &x324 = (x7[x209]);
        std::cout << "(App zipwith ((Id \"(+)\") (Id L15) (Id L16)))" << std::endl;
        auto &x327 = std::get<0>(*x325);
        auto &x326 = std::get<1>(*x325);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x341;
        x341 = std::make_pair(x326, x327);
        int x342 = reconstruct_L(x341);
        auto &x323 = std::get<0>(*x321);
        auto &x322 = std::get<1>(*x321);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x343;
        x343 = std::make_pair(x322, x323);
        int x344 = reconstruct_L(x343);
        // begin Array.init
        x329.clear();
        auto &x323 = std::get<0>(*x321);
        int x328 = (x323).size();
        x329.reserve(x328);
        auto &x323 = std::get<0>(*x321);
        int x328 = (x323).size();
        for (int x330 = 0; x330 < x328; x330++) {

          // begin Array.init
          x336.clear();
          auto &x327 = std::get<0>(*x325);
          auto &x331 = (x327[x330]);
          int x333 = (x331).size();
          auto &x323 = std::get<0>(*x321);
          auto &x332 = (x323[x330]);
          int x334 = (x332).size();
          int x335 = std::min(x334, x333);
          x336.reserve(x335);
          auto &x327 = std::get<0>(*x325);
          auto &x331 = (x327[x330]);
          int x333 = (x331).size();
          auto &x323 = std::get<0>(*x321);
          auto &x332 = (x323[x330]);
          int x334 = (x332).size();
          int x335 = std::min(x334, x333);
          for (int x337 = 0; x337 < x335; x337++) {
            auto &x327 = std::get<0>(*x325);
            auto &x331 = (x327[x330]);
            auto &x338 = (x331[x337]);
            auto &x323 = std::get<0>(*x321);
            auto &x332 = (x323[x330]);
            auto &x339 = (x332[x337]);
            int x340 = (x339 + x338);
            x336.push_back(x340);
          }
          // end Array.init
          x329.push_back(x336);
        }
        // end Array.init
        int x345 = (x329 == x16);
        if (x345) {
          x346 = 0;
        } else {
          x346 = 0;
        }
        for (auto x325 = x324.begin(); x325 != x324.end(); ++x325) {
          std::cout << "(App zipwith ((Id \"(+)\") (Id L15) (Id L16)))"
                    << std::endl;
          auto &x327 = std::get<0>(*x325);
          auto &x326 = std::get<1>(*x325);
          std::pair<std::vector<int>, std::vector<std::vector<int>>> x341;
          x341 = std::make_pair(x326, x327);
          int x342 = reconstruct_L(x341);
          auto &x323 = std::get<0>(*x321);
          auto &x322 = std::get<1>(*x321);
          std::pair<std::vector<int>, std::vector<std::vector<int>>> x343;
          x343 = std::make_pair(x322, x323);
          int x344 = reconstruct_L(x343);
          // begin Array.init
          x329.clear();
          auto &x323 = std::get<0>(*x321);
          int x328 = (x323).size();
          x329.reserve(x328);
          auto &x323 = std::get<0>(*x321);
          int x328 = (x323).size();
          for (int x330 = 0; x330 < x328; x330++) {

            // begin Array.init
            x336.clear();
            auto &x327 = std::get<0>(*x325);
            auto &x331 = (x327[x330]);
            int x333 = (x331).size();
            auto &x323 = std::get<0>(*x321);
            auto &x332 = (x323[x330]);
            int x334 = (x332).size();
            int x335 = std::min(x334, x333);
            x336.reserve(x335);
            auto &x327 = std::get<0>(*x325);
            auto &x331 = (x327[x330]);
            int x333 = (x331).size();
            auto &x323 = std::get<0>(*x321);
            auto &x332 = (x323[x330]);
            int x334 = (x332).size();
            int x335 = std::min(x334, x333);
            for (int x337 = 0; x337 < x335; x337++) {
              auto &x327 = std::get<0>(*x325);
              auto &x331 = (x327[x330]);
              auto &x338 = (x331[x337]);
              auto &x323 = std::get<0>(*x321);
              auto &x332 = (x323[x330]);
              auto &x339 = (x332[x337]);
              int x340 = (x339 + x338);
              x336.push_back(x340);
            }
            // end Array.init
            x329.push_back(x336);
          }
          // end Array.init
          int x345 = (x329 == x16);
          if (x345) {
            x346 = 0;
          } else {
            x346 = 0;
          }
        }
      }
      auto &x347 = (x4[x208]);
      auto &x351 = (x7[x209]);
      std::cout << "(App drop ((Id I2) (Id L3)))" << std::endl;
      auto &x354 = std::get<0>(*x352);
      auto &x353 = std::get<1>(*x352);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x368;
      x368 = std::make_pair(x353, x354);
      int x369 = reconstruct_L(x368);
      auto &x350 = std::get<0>(*x348);
      auto &x349 = std::get<1>(*x348);
      std::pair<std::vector<int>, std::vector<int>> x561;
      x561 = std::make_pair(x349, x350);
      int x562 = reconstruct_I(x561);
      auto &x354 = std::get<0>(*x352);
      auto &x350 = std::get<0>(*x348);
      // begin Array.init
      x356.clear();
      int x355 = (x354).size();
      x356.reserve(x355);
      int x355 = (x354).size();
      for (int x357 = 0; x357 < x355; x357++) {

        // begin Array.init
        x364.clear();
        auto &x358 = (x350[x357]);
        auto &x359 = (x354[x357]);
        int x360 = (x359).size();
        int x361 = (x360 - 1);
        int x362 = (x361 - x358);
        int x363 = std::max(0, x362);
        x364.reserve(x363);
        auto &x358 = (x350[x357]);
        auto &x359 = (x354[x357]);
        int x360 = (x359).size();
        int x361 = (x360 - 1);
        int x362 = (x361 - x358);
        int x363 = std::max(0, x362);
        for (int x365 = 0; x365 < x363; x365++) {
          auto &x358 = (x350[x357]);
          int x366 = (x358 + x365);
          auto &x359 = (x354[x357]);
          auto &x367 = (x359[x366]);
          x364.push_back(x367);
        }
        // end Array.init
        x356.push_back(x364);
      }
      // end Array.init
      int x563 = (x356 == x16);
      if (x563) {
        x564 = 0;
      } else {
        x564 = 0;
      }
      for (auto x352 = x351.begin(); x352 != x351.end(); ++x352) {
        std::cout << "(App drop ((Id I2) (Id L3)))" << std::endl;
        auto &x354 = std::get<0>(*x352);
        auto &x353 = std::get<1>(*x352);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x368;
        x368 = std::make_pair(x353, x354);
        int x369 = reconstruct_L(x368);
        auto &x350 = std::get<0>(*x348);
        auto &x349 = std::get<1>(*x348);
        std::pair<std::vector<int>, std::vector<int>> x561;
        x561 = std::make_pair(x349, x350);
        int x562 = reconstruct_I(x561);
        auto &x354 = std::get<0>(*x352);
        auto &x350 = std::get<0>(*x348);
        // begin Array.init
        x356.clear();
        int x355 = (x354).size();
        x356.reserve(x355);
        int x355 = (x354).size();
        for (int x357 = 0; x357 < x355; x357++) {

          // begin Array.init
          x364.clear();
          auto &x358 = (x350[x357]);
          auto &x359 = (x354[x357]);
          int x360 = (x359).size();
          int x361 = (x360 - 1);
          int x362 = (x361 - x358);
          int x363 = std::max(0, x362);
          x364.reserve(x363);
          auto &x358 = (x350[x357]);
          auto &x359 = (x354[x357]);
          int x360 = (x359).size();
          int x361 = (x360 - 1);
          int x362 = (x361 - x358);
          int x363 = std::max(0, x362);
          for (int x365 = 0; x365 < x363; x365++) {
            auto &x358 = (x350[x357]);
            int x366 = (x358 + x365);
            auto &x359 = (x354[x357]);
            auto &x367 = (x359[x366]);
            x364.push_back(x367);
          }
          // end Array.init
          x356.push_back(x364);
        }
        // end Array.init
        int x563 = (x356 == x16);
        if (x563) {
          x564 = 0;
        } else {
          x564 = 0;
        }
      }
      for (auto x348 = x347.begin(); x348 != x347.end(); ++x348) {
        auto &x351 = (x7[x209]);
        std::cout << "(App drop ((Id I2) (Id L3)))" << std::endl;
        auto &x354 = std::get<0>(*x352);
        auto &x353 = std::get<1>(*x352);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x368;
        x368 = std::make_pair(x353, x354);
        int x369 = reconstruct_L(x368);
        auto &x350 = std::get<0>(*x348);
        auto &x349 = std::get<1>(*x348);
        std::pair<std::vector<int>, std::vector<int>> x561;
        x561 = std::make_pair(x349, x350);
        int x562 = reconstruct_I(x561);
        auto &x354 = std::get<0>(*x352);
        auto &x350 = std::get<0>(*x348);
        // begin Array.init
        x356.clear();
        int x355 = (x354).size();
        x356.reserve(x355);
        int x355 = (x354).size();
        for (int x357 = 0; x357 < x355; x357++) {

          // begin Array.init
          x364.clear();
          auto &x358 = (x350[x357]);
          auto &x359 = (x354[x357]);
          int x360 = (x359).size();
          int x361 = (x360 - 1);
          int x362 = (x361 - x358);
          int x363 = std::max(0, x362);
          x364.reserve(x363);
          auto &x358 = (x350[x357]);
          auto &x359 = (x354[x357]);
          int x360 = (x359).size();
          int x361 = (x360 - 1);
          int x362 = (x361 - x358);
          int x363 = std::max(0, x362);
          for (int x365 = 0; x365 < x363; x365++) {
            auto &x358 = (x350[x357]);
            int x366 = (x358 + x365);
            auto &x359 = (x354[x357]);
            auto &x367 = (x359[x366]);
            x364.push_back(x367);
          }
          // end Array.init
          x356.push_back(x364);
        }
        // end Array.init
        int x563 = (x356 == x16);
        if (x563) {
          x564 = 0;
        } else {
          x564 = 0;
        }
        for (auto x352 = x351.begin(); x352 != x351.end(); ++x352) {
          std::cout << "(App drop ((Id I2) (Id L3)))" << std::endl;
          auto &x354 = std::get<0>(*x352);
          auto &x353 = std::get<1>(*x352);
          std::pair<std::vector<int>, std::vector<std::vector<int>>> x368;
          x368 = std::make_pair(x353, x354);
          int x369 = reconstruct_L(x368);
          auto &x350 = std::get<0>(*x348);
          auto &x349 = std::get<1>(*x348);
          std::pair<std::vector<int>, std::vector<int>> x561;
          x561 = std::make_pair(x349, x350);
          int x562 = reconstruct_I(x561);
          auto &x354 = std::get<0>(*x352);
          auto &x350 = std::get<0>(*x348);
          // begin Array.init
          x356.clear();
          int x355 = (x354).size();
          x356.reserve(x355);
          int x355 = (x354).size();
          for (int x357 = 0; x357 < x355; x357++) {

            // begin Array.init
            x364.clear();
            auto &x358 = (x350[x357]);
            auto &x359 = (x354[x357]);
            int x360 = (x359).size();
            int x361 = (x360 - 1);
            int x362 = (x361 - x358);
            int x363 = std::max(0, x362);
            x364.reserve(x363);
            auto &x358 = (x350[x357]);
            auto &x359 = (x354[x357]);
            int x360 = (x359).size();
            int x361 = (x360 - 1);
            int x362 = (x361 - x358);
            int x363 = std::max(0, x362);
            for (int x365 = 0; x365 < x363; x365++) {
              auto &x358 = (x350[x357]);
              int x366 = (x358 + x365);
              auto &x359 = (x354[x357]);
              auto &x367 = (x359[x366]);
              x364.push_back(x367);
            }
            // end Array.init
            x356.push_back(x364);
          }
          // end Array.init
          int x563 = (x356 == x16);
          if (x563) {
            x564 = 0;
          } else {
            x564 = 0;
          }
        }
      }
      auto &x565 = (x4[x208]);
      auto &x569 = (x7[x209]);
      std::cout << "(App take ((Id I0) (Id L1)))" << std::endl;
      auto &x572 = std::get<0>(*x570);
      auto &x571 = std::get<1>(*x570);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x584;
      x584 = std::make_pair(x571, x572);
      int x585 = reconstruct_L(x584);
      auto &x568 = std::get<0>(*x566);
      auto &x567 = std::get<1>(*x566);
      std::pair<std::vector<int>, std::vector<int>> x586;
      x586 = std::make_pair(x567, x568);
      int x587 = reconstruct_I(x586);
      // begin Array.init
      x574.clear();
      auto &x572 = std::get<0>(*x570);
      int x573 = (x572).size();
      x574.reserve(x573);
      auto &x572 = std::get<0>(*x570);
      int x573 = (x572).size();
      for (int x575 = 0; x575 < x573; x575++) {

        // begin Array.init
        x580.clear();
        auto &x568 = std::get<0>(*x566);
        auto &x576 = (x568[x575]);
        int x578 = (x576 - 0);
        int x579 = std::max(0, x578);
        x580.reserve(x579);
        auto &x568 = std::get<0>(*x566);
        auto &x576 = (x568[x575]);
        int x578 = (x576 - 0);
        int x579 = std::max(0, x578);
        for (int x581 = 0; x581 < x579; x581++) {
          int x582 = (0 + x581);
          auto &x572 = std::get<0>(*x570);
          auto &x577 = (x572[x575]);
          auto &x583 = (x577[x582]);
          x580.push_back(x583);
        }
        // end Array.init
        x574.push_back(x580);
      }
      // end Array.init
      int x588 = (x574 == x16);
      if (x588) {
        x589 = 0;
      } else {
        x589 = 0;
      }
      for (auto x570 = x569.begin(); x570 != x569.end(); ++x570) {
        std::cout << "(App take ((Id I0) (Id L1)))" << std::endl;
        auto &x572 = std::get<0>(*x570);
        auto &x571 = std::get<1>(*x570);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x584;
        x584 = std::make_pair(x571, x572);
        int x585 = reconstruct_L(x584);
        auto &x568 = std::get<0>(*x566);
        auto &x567 = std::get<1>(*x566);
        std::pair<std::vector<int>, std::vector<int>> x586;
        x586 = std::make_pair(x567, x568);
        int x587 = reconstruct_I(x586);
        // begin Array.init
        x574.clear();
        auto &x572 = std::get<0>(*x570);
        int x573 = (x572).size();
        x574.reserve(x573);
        auto &x572 = std::get<0>(*x570);
        int x573 = (x572).size();
        for (int x575 = 0; x575 < x573; x575++) {

          // begin Array.init
          x580.clear();
          auto &x568 = std::get<0>(*x566);
          auto &x576 = (x568[x575]);
          int x578 = (x576 - 0);
          int x579 = std::max(0, x578);
          x580.reserve(x579);
          auto &x568 = std::get<0>(*x566);
          auto &x576 = (x568[x575]);
          int x578 = (x576 - 0);
          int x579 = std::max(0, x578);
          for (int x581 = 0; x581 < x579; x581++) {
            int x582 = (0 + x581);
            auto &x572 = std::get<0>(*x570);
            auto &x577 = (x572[x575]);
            auto &x583 = (x577[x582]);
            x580.push_back(x583);
          }
          // end Array.init
          x574.push_back(x580);
        }
        // end Array.init
        int x588 = (x574 == x16);
        if (x588) {
          x589 = 0;
        } else {
          x589 = 0;
        }
      }
      for (auto x566 = x565.begin(); x566 != x565.end(); ++x566) {
        auto &x569 = (x7[x209]);
        std::cout << "(App take ((Id I0) (Id L1)))" << std::endl;
        auto &x572 = std::get<0>(*x570);
        auto &x571 = std::get<1>(*x570);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x584;
        x584 = std::make_pair(x571, x572);
        int x585 = reconstruct_L(x584);
        auto &x568 = std::get<0>(*x566);
        auto &x567 = std::get<1>(*x566);
        std::pair<std::vector<int>, std::vector<int>> x586;
        x586 = std::make_pair(x567, x568);
        int x587 = reconstruct_I(x586);
        // begin Array.init
        x574.clear();
        auto &x572 = std::get<0>(*x570);
        int x573 = (x572).size();
        x574.reserve(x573);
        auto &x572 = std::get<0>(*x570);
        int x573 = (x572).size();
        for (int x575 = 0; x575 < x573; x575++) {

          // begin Array.init
          x580.clear();
          auto &x568 = std::get<0>(*x566);
          auto &x576 = (x568[x575]);
          int x578 = (x576 - 0);
          int x579 = std::max(0, x578);
          x580.reserve(x579);
          auto &x568 = std::get<0>(*x566);
          auto &x576 = (x568[x575]);
          int x578 = (x576 - 0);
          int x579 = std::max(0, x578);
          for (int x581 = 0; x581 < x579; x581++) {
            int x582 = (0 + x581);
            auto &x572 = std::get<0>(*x570);
            auto &x577 = (x572[x575]);
            auto &x583 = (x577[x582]);
            x580.push_back(x583);
          }
          // end Array.init
          x574.push_back(x580);
        }
        // end Array.init
        int x588 = (x574 == x16);
        if (x588) {
          x589 = 0;
        } else {
          x589 = 0;
        }
        for (auto x570 = x569.begin(); x570 != x569.end(); ++x570) {
          std::cout << "(App take ((Id I0) (Id L1)))" << std::endl;
          auto &x572 = std::get<0>(*x570);
          auto &x571 = std::get<1>(*x570);
          std::pair<std::vector<int>, std::vector<std::vector<int>>> x584;
          x584 = std::make_pair(x571, x572);
          int x585 = reconstruct_L(x584);
          auto &x568 = std::get<0>(*x566);
          auto &x567 = std::get<1>(*x566);
          std::pair<std::vector<int>, std::vector<int>> x586;
          x586 = std::make_pair(x567, x568);
          int x587 = reconstruct_I(x586);
          // begin Array.init
          x574.clear();
          auto &x572 = std::get<0>(*x570);
          int x573 = (x572).size();
          x574.reserve(x573);
          auto &x572 = std::get<0>(*x570);
          int x573 = (x572).size();
          for (int x575 = 0; x575 < x573; x575++) {

            // begin Array.init
            x580.clear();
            auto &x568 = std::get<0>(*x566);
            auto &x576 = (x568[x575]);
            int x578 = (x576 - 0);
            int x579 = std::max(0, x578);
            x580.reserve(x579);
            auto &x568 = std::get<0>(*x566);
            auto &x576 = (x568[x575]);
            int x578 = (x576 - 0);
            int x579 = std::max(0, x578);
            for (int x581 = 0; x581 < x579; x581++) {
              int x582 = (0 + x581);
              auto &x572 = std::get<0>(*x570);
              auto &x577 = (x572[x575]);
              auto &x583 = (x577[x582]);
              x580.push_back(x583);
            }
            // end Array.init
            x574.push_back(x580);
          }
          // end Array.init
          int x588 = (x574 == x16);
          if (x588) {
            x589 = 0;
          } else {
            x589 = 0;
          }
        }
      }
      int x590 = (x15).size();
      int x591 = (x590 == 2);
      if (x591) {
        x592 = 0;
      } else {
        x592 = 0;
      }
      auto &x19 = (x15[0]);
      auto &x20 = (x7[x19]);
      std::cout << "(App map ((Id \"(/4)\") (Id L14)))" << std::endl;
      auto &x23 = std::get<0>(*x21);
      auto &x22 = std::get<1>(*x21);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x33;
      x33 = std::make_pair(x22, x23);
      int x34 = reconstruct_L(x33);
      // begin Array.init
      x25.clear();
      auto &x23 = std::get<0>(*x21);
      int x24 = (x23).size();
      x25.reserve(x24);
      auto &x23 = std::get<0>(*x21);
      int x24 = (x23).size();
      for (int x26 = 0; x26 < x24; x26++) {
        auto &x23 = std::get<0>(*x21);
        auto &x27 = (x23[x26]);
        // begin Array.init
        x29.clear();
        int x28 = (x27).size();
        x29.reserve(x28);
        int x28 = (x27).size();
        for (int x30 = 0; x30 < x28; x30++) {
          auto &x31 = (x27[x30]);
          int x32 = (x31 / 4);
          x29.push_back(x32);
        }
        // end Array.init
        x25.push_back(x29);
      }
      // end Array.init
      int x35 = (x25 == x16);
      if (x35) {
        x36 = 0;
      } else {
        x36 = 0;
      }
      for (auto x21 = x20.begin(); x21 != x20.end(); ++x21) {
        std::cout << "(App map ((Id \"(/4)\") (Id L14)))" << std::endl;
        auto &x23 = std::get<0>(*x21);
        auto &x22 = std::get<1>(*x21);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x33;
        x33 = std::make_pair(x22, x23);
        int x34 = reconstruct_L(x33);
        // begin Array.init
        x25.clear();
        auto &x23 = std::get<0>(*x21);
        int x24 = (x23).size();
        x25.reserve(x24);
        auto &x23 = std::get<0>(*x21);
        int x24 = (x23).size();
        for (int x26 = 0; x26 < x24; x26++) {
          auto &x23 = std::get<0>(*x21);
          auto &x27 = (x23[x26]);
          // begin Array.init
          x29.clear();
          int x28 = (x27).size();
          x29.reserve(x28);
          int x28 = (x27).size();
          for (int x30 = 0; x30 < x28; x30++) {
            auto &x31 = (x27[x30]);
            int x32 = (x31 / 4);
            x29.push_back(x32);
          }
          // end Array.init
          x25.push_back(x29);
        }
        // end Array.init
        int x35 = (x25 == x16);
        if (x35) {
          x36 = 0;
        } else {
          x36 = 0;
        }
      }
      auto &x37 = (x7[x19]);
      std::cout << "(App map ((Id \"(*4)\") (Id L13)))" << std::endl;
      auto &x40 = std::get<0>(*x38);
      auto &x39 = std::get<1>(*x38);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x50;
      x50 = std::make_pair(x39, x40);
      int x51 = reconstruct_L(x50);
      // begin Array.init
      x42.clear();
      auto &x40 = std::get<0>(*x38);
      int x41 = (x40).size();
      x42.reserve(x41);
      auto &x40 = std::get<0>(*x38);
      int x41 = (x40).size();
      for (int x43 = 0; x43 < x41; x43++) {
        auto &x40 = std::get<0>(*x38);
        auto &x44 = (x40[x43]);
        // begin Array.init
        x46.clear();
        int x45 = (x44).size();
        x46.reserve(x45);
        int x45 = (x44).size();
        for (int x47 = 0; x47 < x45; x47++) {
          auto &x48 = (x44[x47]);
          int x49 = (x48 * 4);
          x46.push_back(x49);
        }
        // end Array.init
        x42.push_back(x46);
      }
      // end Array.init
      int x52 = (x42 == x16);
      if (x52) {
        x53 = 0;
      } else {
        x53 = 0;
      }
      for (auto x38 = x37.begin(); x38 != x37.end(); ++x38) {
        std::cout << "(App map ((Id \"(*4)\") (Id L13)))" << std::endl;
        auto &x40 = std::get<0>(*x38);
        auto &x39 = std::get<1>(*x38);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x50;
        x50 = std::make_pair(x39, x40);
        int x51 = reconstruct_L(x50);
        // begin Array.init
        x42.clear();
        auto &x40 = std::get<0>(*x38);
        int x41 = (x40).size();
        x42.reserve(x41);
        auto &x40 = std::get<0>(*x38);
        int x41 = (x40).size();
        for (int x43 = 0; x43 < x41; x43++) {
          auto &x40 = std::get<0>(*x38);
          auto &x44 = (x40[x43]);
          // begin Array.init
          x46.clear();
          int x45 = (x44).size();
          x46.reserve(x45);
          int x45 = (x44).size();
          for (int x47 = 0; x47 < x45; x47++) {
            auto &x48 = (x44[x47]);
            int x49 = (x48 * 4);
            x46.push_back(x49);
          }
          // end Array.init
          x42.push_back(x46);
        }
        // end Array.init
        int x52 = (x42 == x16);
        if (x52) {
          x53 = 0;
        } else {
          x53 = 0;
        }
      }
      auto &x54 = (x7[x19]);
      std::cout << "(App map ((Id \"(/3)\") (Id L12)))" << std::endl;
      auto &x57 = std::get<0>(*x55);
      auto &x56 = std::get<1>(*x55);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x67;
      x67 = std::make_pair(x56, x57);
      int x68 = reconstruct_L(x67);
      // begin Array.init
      x59.clear();
      auto &x57 = std::get<0>(*x55);
      int x58 = (x57).size();
      x59.reserve(x58);
      auto &x57 = std::get<0>(*x55);
      int x58 = (x57).size();
      for (int x60 = 0; x60 < x58; x60++) {
        auto &x57 = std::get<0>(*x55);
        auto &x61 = (x57[x60]);
        // begin Array.init
        x63.clear();
        int x62 = (x61).size();
        x63.reserve(x62);
        int x62 = (x61).size();
        for (int x64 = 0; x64 < x62; x64++) {
          auto &x65 = (x61[x64]);
          int x66 = (x65 / 3);
          x63.push_back(x66);
        }
        // end Array.init
        x59.push_back(x63);
      }
      // end Array.init
      int x69 = (x59 == x16);
      if (x69) {
        x70 = 0;
      } else {
        x70 = 0;
      }
      for (auto x55 = x54.begin(); x55 != x54.end(); ++x55) {
        std::cout << "(App map ((Id \"(/3)\") (Id L12)))" << std::endl;
        auto &x57 = std::get<0>(*x55);
        auto &x56 = std::get<1>(*x55);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x67;
        x67 = std::make_pair(x56, x57);
        int x68 = reconstruct_L(x67);
        // begin Array.init
        x59.clear();
        auto &x57 = std::get<0>(*x55);
        int x58 = (x57).size();
        x59.reserve(x58);
        auto &x57 = std::get<0>(*x55);
        int x58 = (x57).size();
        for (int x60 = 0; x60 < x58; x60++) {
          auto &x57 = std::get<0>(*x55);
          auto &x61 = (x57[x60]);
          // begin Array.init
          x63.clear();
          int x62 = (x61).size();
          x63.reserve(x62);
          int x62 = (x61).size();
          for (int x64 = 0; x64 < x62; x64++) {
            auto &x65 = (x61[x64]);
            int x66 = (x65 / 3);
            x63.push_back(x66);
          }
          // end Array.init
          x59.push_back(x63);
        }
        // end Array.init
        int x69 = (x59 == x16);
        if (x69) {
          x70 = 0;
        } else {
          x70 = 0;
        }
      }
      auto &x71 = (x7[x19]);
      std::cout << "(App map ((Id \"(*3)\") (Id L11)))" << std::endl;
      auto &x74 = std::get<0>(*x72);
      auto &x73 = std::get<1>(*x72);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x84;
      x84 = std::make_pair(x73, x74);
      int x85 = reconstruct_L(x84);
      // begin Array.init
      x76.clear();
      auto &x74 = std::get<0>(*x72);
      int x75 = (x74).size();
      x76.reserve(x75);
      auto &x74 = std::get<0>(*x72);
      int x75 = (x74).size();
      for (int x77 = 0; x77 < x75; x77++) {
        auto &x74 = std::get<0>(*x72);
        auto &x78 = (x74[x77]);
        // begin Array.init
        x80.clear();
        int x79 = (x78).size();
        x80.reserve(x79);
        int x79 = (x78).size();
        for (int x81 = 0; x81 < x79; x81++) {
          auto &x82 = (x78[x81]);
          int x83 = (x82 * 3);
          x80.push_back(x83);
        }
        // end Array.init
        x76.push_back(x80);
      }
      // end Array.init
      int x86 = (x76 == x16);
      if (x86) {
        x87 = 0;
      } else {
        x87 = 0;
      }
      for (auto x72 = x71.begin(); x72 != x71.end(); ++x72) {
        std::cout << "(App map ((Id \"(*3)\") (Id L11)))" << std::endl;
        auto &x74 = std::get<0>(*x72);
        auto &x73 = std::get<1>(*x72);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x84;
        x84 = std::make_pair(x73, x74);
        int x85 = reconstruct_L(x84);
        // begin Array.init
        x76.clear();
        auto &x74 = std::get<0>(*x72);
        int x75 = (x74).size();
        x76.reserve(x75);
        auto &x74 = std::get<0>(*x72);
        int x75 = (x74).size();
        for (int x77 = 0; x77 < x75; x77++) {
          auto &x74 = std::get<0>(*x72);
          auto &x78 = (x74[x77]);
          // begin Array.init
          x80.clear();
          int x79 = (x78).size();
          x80.reserve(x79);
          int x79 = (x78).size();
          for (int x81 = 0; x81 < x79; x81++) {
            auto &x82 = (x78[x81]);
            int x83 = (x82 * 3);
            x80.push_back(x83);
          }
          // end Array.init
          x76.push_back(x80);
        }
        // end Array.init
        int x86 = (x76 == x16);
        if (x86) {
          x87 = 0;
        } else {
          x87 = 0;
        }
      }
      auto &x88 = (x7[x19]);
      std::cout << "(App map ((Id \"(**2)\") (Id L10)))" << std::endl;
      auto &x91 = std::get<0>(*x89);
      auto &x90 = std::get<1>(*x89);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x101;
      x101 = std::make_pair(x90, x91);
      int x102 = reconstruct_L(x101);
      // begin Array.init
      x93.clear();
      auto &x91 = std::get<0>(*x89);
      int x92 = (x91).size();
      x93.reserve(x92);
      auto &x91 = std::get<0>(*x89);
      int x92 = (x91).size();
      for (int x94 = 0; x94 < x92; x94++) {
        auto &x91 = std::get<0>(*x89);
        auto &x95 = (x91[x94]);
        // begin Array.init
        x97.clear();
        int x96 = (x95).size();
        x97.reserve(x96);
        int x96 = (x95).size();
        for (int x98 = 0; x98 < x96; x98++) {
          auto &x99 = (x95[x98]);
          int x100 = (x99 * x99);
          x97.push_back(x100);
        }
        // end Array.init
        x93.push_back(x97);
      }
      // end Array.init
      int x103 = (x93 == x16);
      if (x103) {
        x104 = 0;
      } else {
        x104 = 0;
      }
      for (auto x89 = x88.begin(); x89 != x88.end(); ++x89) {
        std::cout << "(App map ((Id \"(**2)\") (Id L10)))" << std::endl;
        auto &x91 = std::get<0>(*x89);
        auto &x90 = std::get<1>(*x89);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x101;
        x101 = std::make_pair(x90, x91);
        int x102 = reconstruct_L(x101);
        // begin Array.init
        x93.clear();
        auto &x91 = std::get<0>(*x89);
        int x92 = (x91).size();
        x93.reserve(x92);
        auto &x91 = std::get<0>(*x89);
        int x92 = (x91).size();
        for (int x94 = 0; x94 < x92; x94++) {
          auto &x91 = std::get<0>(*x89);
          auto &x95 = (x91[x94]);
          // begin Array.init
          x97.clear();
          int x96 = (x95).size();
          x97.reserve(x96);
          int x96 = (x95).size();
          for (int x98 = 0; x98 < x96; x98++) {
            auto &x99 = (x95[x98]);
            int x100 = (x99 * x99);
            x97.push_back(x100);
          }
          // end Array.init
          x93.push_back(x97);
        }
        // end Array.init
        int x103 = (x93 == x16);
        if (x103) {
          x104 = 0;
        } else {
          x104 = 0;
        }
      }
      auto &x105 = (x7[x19]);
      std::cout << "(App map ((Id \"(*(-1))\") (Id L9)))" << std::endl;
      auto &x108 = std::get<0>(*x106);
      auto &x107 = std::get<1>(*x106);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x118;
      x118 = std::make_pair(x107, x108);
      int x119 = reconstruct_L(x118);
      // begin Array.init
      x110.clear();
      auto &x108 = std::get<0>(*x106);
      int x109 = (x108).size();
      x110.reserve(x109);
      auto &x108 = std::get<0>(*x106);
      int x109 = (x108).size();
      for (int x111 = 0; x111 < x109; x111++) {
        auto &x108 = std::get<0>(*x106);
        auto &x112 = (x108[x111]);
        // begin Array.init
        x114.clear();
        int x113 = (x112).size();
        x114.reserve(x113);
        int x113 = (x112).size();
        for (int x115 = 0; x115 < x113; x115++) {
          auto &x116 = (x112[x115]);
          int x117 = (-x116);
          x114.push_back(x117);
        }
        // end Array.init
        x110.push_back(x114);
      }
      // end Array.init
      int x120 = (x110 == x16);
      if (x120) {
        x121 = 0;
      } else {
        x121 = 0;
      }
      for (auto x106 = x105.begin(); x106 != x105.end(); ++x106) {
        std::cout << "(App map ((Id \"(*(-1))\") (Id L9)))" << std::endl;
        auto &x108 = std::get<0>(*x106);
        auto &x107 = std::get<1>(*x106);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x118;
        x118 = std::make_pair(x107, x108);
        int x119 = reconstruct_L(x118);
        // begin Array.init
        x110.clear();
        auto &x108 = std::get<0>(*x106);
        int x109 = (x108).size();
        x110.reserve(x109);
        auto &x108 = std::get<0>(*x106);
        int x109 = (x108).size();
        for (int x111 = 0; x111 < x109; x111++) {
          auto &x108 = std::get<0>(*x106);
          auto &x112 = (x108[x111]);
          // begin Array.init
          x114.clear();
          int x113 = (x112).size();
          x114.reserve(x113);
          int x113 = (x112).size();
          for (int x115 = 0; x115 < x113; x115++) {
            auto &x116 = (x112[x115]);
            int x117 = (-x116);
            x114.push_back(x117);
          }
          // end Array.init
          x110.push_back(x114);
        }
        // end Array.init
        int x120 = (x110 == x16);
        if (x120) {
          x121 = 0;
        } else {
          x121 = 0;
        }
      }
      auto &x122 = (x7[x19]);
      std::cout << "(App map ((Id \"(/2)\") (Id L8)))" << std::endl;
      auto &x125 = std::get<0>(*x123);
      auto &x124 = std::get<1>(*x123);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x135;
      x135 = std::make_pair(x124, x125);
      int x136 = reconstruct_L(x135);
      // begin Array.init
      x127.clear();
      auto &x125 = std::get<0>(*x123);
      int x126 = (x125).size();
      x127.reserve(x126);
      auto &x125 = std::get<0>(*x123);
      int x126 = (x125).size();
      for (int x128 = 0; x128 < x126; x128++) {
        auto &x125 = std::get<0>(*x123);
        auto &x129 = (x125[x128]);
        // begin Array.init
        x131.clear();
        int x130 = (x129).size();
        x131.reserve(x130);
        int x130 = (x129).size();
        for (int x132 = 0; x132 < x130; x132++) {
          auto &x133 = (x129[x132]);
          int x134 = (x133 / 2);
          x131.push_back(x134);
        }
        // end Array.init
        x127.push_back(x131);
      }
      // end Array.init
      int x137 = (x127 == x16);
      if (x137) {
        x138 = 0;
      } else {
        x138 = 0;
      }
      for (auto x123 = x122.begin(); x123 != x122.end(); ++x123) {
        std::cout << "(App map ((Id \"(/2)\") (Id L8)))" << std::endl;
        auto &x125 = std::get<0>(*x123);
        auto &x124 = std::get<1>(*x123);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x135;
        x135 = std::make_pair(x124, x125);
        int x136 = reconstruct_L(x135);
        // begin Array.init
        x127.clear();
        auto &x125 = std::get<0>(*x123);
        int x126 = (x125).size();
        x127.reserve(x126);
        auto &x125 = std::get<0>(*x123);
        int x126 = (x125).size();
        for (int x128 = 0; x128 < x126; x128++) {
          auto &x125 = std::get<0>(*x123);
          auto &x129 = (x125[x128]);
          // begin Array.init
          x131.clear();
          int x130 = (x129).size();
          x131.reserve(x130);
          int x130 = (x129).size();
          for (int x132 = 0; x132 < x130; x132++) {
            auto &x133 = (x129[x132]);
            int x134 = (x133 / 2);
            x131.push_back(x134);
          }
          // end Array.init
          x127.push_back(x131);
        }
        // end Array.init
        int x137 = (x127 == x16);
        if (x137) {
          x138 = 0;
        } else {
          x138 = 0;
        }
      }
      auto &x139 = (x7[x19]);
      std::cout << "(App map ((Id \"(*2)\") (Id L7)))" << std::endl;
      auto &x142 = std::get<0>(*x140);
      auto &x141 = std::get<1>(*x140);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x152;
      x152 = std::make_pair(x141, x142);
      int x153 = reconstruct_L(x152);
      // begin Array.init
      x144.clear();
      auto &x142 = std::get<0>(*x140);
      int x143 = (x142).size();
      x144.reserve(x143);
      auto &x142 = std::get<0>(*x140);
      int x143 = (x142).size();
      for (int x145 = 0; x145 < x143; x145++) {
        auto &x142 = std::get<0>(*x140);
        auto &x146 = (x142[x145]);
        // begin Array.init
        x148.clear();
        int x147 = (x146).size();
        x148.reserve(x147);
        int x147 = (x146).size();
        for (int x149 = 0; x149 < x147; x149++) {
          auto &x150 = (x146[x149]);
          int x151 = (x150 * 2);
          x148.push_back(x151);
        }
        // end Array.init
        x144.push_back(x148);
      }
      // end Array.init
      int x154 = (x144 == x16);
      if (x154) {
        x155 = 0;
      } else {
        x155 = 0;
      }
      for (auto x140 = x139.begin(); x140 != x139.end(); ++x140) {
        std::cout << "(App map ((Id \"(*2)\") (Id L7)))" << std::endl;
        auto &x142 = std::get<0>(*x140);
        auto &x141 = std::get<1>(*x140);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x152;
        x152 = std::make_pair(x141, x142);
        int x153 = reconstruct_L(x152);
        // begin Array.init
        x144.clear();
        auto &x142 = std::get<0>(*x140);
        int x143 = (x142).size();
        x144.reserve(x143);
        auto &x142 = std::get<0>(*x140);
        int x143 = (x142).size();
        for (int x145 = 0; x145 < x143; x145++) {
          auto &x142 = std::get<0>(*x140);
          auto &x146 = (x142[x145]);
          // begin Array.init
          x148.clear();
          int x147 = (x146).size();
          x148.reserve(x147);
          int x147 = (x146).size();
          for (int x149 = 0; x149 < x147; x149++) {
            auto &x150 = (x146[x149]);
            int x151 = (x150 * 2);
            x148.push_back(x151);
          }
          // end Array.init
          x144.push_back(x148);
        }
        // end Array.init
        int x154 = (x144 == x16);
        if (x154) {
          x155 = 0;
        } else {
          x155 = 0;
        }
      }
      auto &x156 = (x7[x19]);
      std::cout << "(App map ((Id \"(-1)\") (Id L6)))" << std::endl;
      auto &x159 = std::get<0>(*x157);
      auto &x158 = std::get<1>(*x157);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x169;
      x169 = std::make_pair(x158, x159);
      int x170 = reconstruct_L(x169);
      // begin Array.init
      x161.clear();
      auto &x159 = std::get<0>(*x157);
      int x160 = (x159).size();
      x161.reserve(x160);
      auto &x159 = std::get<0>(*x157);
      int x160 = (x159).size();
      for (int x162 = 0; x162 < x160; x162++) {
        auto &x159 = std::get<0>(*x157);
        auto &x163 = (x159[x162]);
        // begin Array.init
        x165.clear();
        int x164 = (x163).size();
        x165.reserve(x164);
        int x164 = (x163).size();
        for (int x166 = 0; x166 < x164; x166++) {
          auto &x167 = (x163[x166]);
          int x168 = (x167 - 1);
          x165.push_back(x168);
        }
        // end Array.init
        x161.push_back(x165);
      }
      // end Array.init
      int x171 = (x161 == x16);
      if (x171) {
        x172 = 0;
      } else {
        x172 = 0;
      }
      for (auto x157 = x156.begin(); x157 != x156.end(); ++x157) {
        std::cout << "(App map ((Id \"(-1)\") (Id L6)))" << std::endl;
        auto &x159 = std::get<0>(*x157);
        auto &x158 = std::get<1>(*x157);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x169;
        x169 = std::make_pair(x158, x159);
        int x170 = reconstruct_L(x169);
        // begin Array.init
        x161.clear();
        auto &x159 = std::get<0>(*x157);
        int x160 = (x159).size();
        x161.reserve(x160);
        auto &x159 = std::get<0>(*x157);
        int x160 = (x159).size();
        for (int x162 = 0; x162 < x160; x162++) {
          auto &x159 = std::get<0>(*x157);
          auto &x163 = (x159[x162]);
          // begin Array.init
          x165.clear();
          int x164 = (x163).size();
          x165.reserve(x164);
          int x164 = (x163).size();
          for (int x166 = 0; x166 < x164; x166++) {
            auto &x167 = (x163[x166]);
            int x168 = (x167 - 1);
            x165.push_back(x168);
          }
          // end Array.init
          x161.push_back(x165);
        }
        // end Array.init
        int x171 = (x161 == x16);
        if (x171) {
          x172 = 0;
        } else {
          x172 = 0;
        }
      }
      auto &x173 = (x7[x19]);
      std::cout << "(App map ((Id \"(+1)\") (Id L5)))" << std::endl;
      auto &x176 = std::get<0>(*x174);
      auto &x175 = std::get<1>(*x174);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x186;
      x186 = std::make_pair(x175, x176);
      int x187 = reconstruct_L(x186);
      // begin Array.init
      x178.clear();
      auto &x176 = std::get<0>(*x174);
      int x177 = (x176).size();
      x178.reserve(x177);
      auto &x176 = std::get<0>(*x174);
      int x177 = (x176).size();
      for (int x179 = 0; x179 < x177; x179++) {
        auto &x176 = std::get<0>(*x174);
        auto &x180 = (x176[x179]);
        // begin Array.init
        x182.clear();
        int x181 = (x180).size();
        x182.reserve(x181);
        int x181 = (x180).size();
        for (int x183 = 0; x183 < x181; x183++) {
          auto &x184 = (x180[x183]);
          int x185 = (x184 + 1);
          x182.push_back(x185);
        }
        // end Array.init
        x178.push_back(x182);
      }
      // end Array.init
      int x188 = (x178 == x16);
      if (x188) {
        x189 = 0;
      } else {
        x189 = 0;
      }
      for (auto x174 = x173.begin(); x174 != x173.end(); ++x174) {
        std::cout << "(App map ((Id \"(+1)\") (Id L5)))" << std::endl;
        auto &x176 = std::get<0>(*x174);
        auto &x175 = std::get<1>(*x174);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x186;
        x186 = std::make_pair(x175, x176);
        int x187 = reconstruct_L(x186);
        // begin Array.init
        x178.clear();
        auto &x176 = std::get<0>(*x174);
        int x177 = (x176).size();
        x178.reserve(x177);
        auto &x176 = std::get<0>(*x174);
        int x177 = (x176).size();
        for (int x179 = 0; x179 < x177; x179++) {
          auto &x176 = std::get<0>(*x174);
          auto &x180 = (x176[x179]);
          // begin Array.init
          x182.clear();
          int x181 = (x180).size();
          x182.reserve(x181);
          int x181 = (x180).size();
          for (int x183 = 0; x183 < x181; x183++) {
            auto &x184 = (x180[x183]);
            int x185 = (x184 + 1);
            x182.push_back(x185);
          }
          // end Array.init
          x178.push_back(x182);
        }
        // end Array.init
        int x188 = (x178 == x16);
        if (x188) {
          x189 = 0;
        } else {
          x189 = 0;
        }
      }
      auto &x190 = (x7[x19]);
      std::cout << "(App reverse ((Id L4)))" << std::endl;
      auto &x193 = std::get<0>(*x191);
      auto &x192 = std::get<1>(*x191);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x204;
      x204 = std::make_pair(x192, x193);
      int x205 = reconstruct_L(x204);
      // begin Array.init
      x195.clear();
      auto &x193 = std::get<0>(*x191);
      int x194 = (x193).size();
      x195.reserve(x194);
      auto &x193 = std::get<0>(*x191);
      int x194 = (x193).size();
      for (int x196 = 0; x196 < x194; x196++) {
        auto &x193 = std::get<0>(*x191);
        auto &x197 = (x193[x196]);
        int x198 = (x197).size();
        // begin Array.init
        x199.clear();
        x199.reserve(x198);
        for (int x200 = 0; x200 < x198; x200++) {
          int x201 = (x198 - x200);
          int x202 = (x201 - 1);
          auto &x203 = (x197[x202]);
          x199.push_back(x203);
        }
        // end Array.init
        x195.push_back(x199);
      }
      // end Array.init
      int x206 = (x195 == x16);
      if (x206) {
        x207 = 0;
      } else {
        x207 = 0;
      }
      for (auto x191 = x190.begin(); x191 != x190.end(); ++x191) {
        std::cout << "(App reverse ((Id L4)))" << std::endl;
        auto &x193 = std::get<0>(*x191);
        auto &x192 = std::get<1>(*x191);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x204;
        x204 = std::make_pair(x192, x193);
        int x205 = reconstruct_L(x204);
        // begin Array.init
        x195.clear();
        auto &x193 = std::get<0>(*x191);
        int x194 = (x193).size();
        x195.reserve(x194);
        auto &x193 = std::get<0>(*x191);
        int x194 = (x193).size();
        for (int x196 = 0; x196 < x194; x196++) {
          auto &x193 = std::get<0>(*x191);
          auto &x197 = (x193[x196]);
          int x198 = (x197).size();
          // begin Array.init
          x199.clear();
          x199.reserve(x198);
          for (int x200 = 0; x200 < x198; x200++) {
            int x201 = (x198 - x200);
            int x202 = (x201 - 1);
            auto &x203 = (x197[x202]);
            x199.push_back(x203);
          }
          // end Array.init
          x195.push_back(x199);
        }
        // end Array.init
        int x206 = (x195 == x16);
        if (x206) {
          x207 = 0;
        } else {
          x207 = 0;
        }
      }
      int x593 = (x15).size();
      int x594 = (x593 == 1);
      if (x594) {
        x595 = 0;
      } else {
        x595 = x592;
      }
      std::cout << "(Id i0)" << std::endl;
      // begin Array.const

      // begin Array.const
      x0[0] = 3;
      x0[1] = 7;
      x0[2] = 5;
      x0[3] = 2;
      x0[4] = 8;  // end Array.const
      x1[0] = x0; // end Array.const
      int x17 = (x1 == x16);
      if (x17) {
        x18 = 0;
      } else {
        x18 = 0;
      }
      int x596 = (x15).size();
      int x597 = (x596 == 0);
      if (x597) {
        x598 = x18;
      } else {
        x598 = x595;
      }
      return x598;
    }
    int reconstruct_I(std::pair<std::vector<int>, std::vector<int>> &x370) {
      std::vector<int> x379;
      int x382;
      int x389;
      int x393;
      std::vector<int> x399;
      int x402;
      int x409;
      int x413;
      std::vector<int> x419;
      int x422;
      int x428;
      int x432;
      std::vector<int> x438;
      int x441;
      int x447;
      int x451;
      std::vector<int> x457;
      int x460;
      int x468;
      std::vector<int> x474;
      int x477;
      int x482;
      int x486;
      std::vector<int> x492;
      int x495;
      int x500;
      int x504;
      std::vector<int> x510;
      int x519;
      std::vector<int> x525;
      int x532;
      std::vector<int> x544;
      int x554;
      int x557;
      int x560;
      auto &x371 = std::get<0>(x370);
      auto &x372 = std::get<1>(x370);
      auto &x534 = (x371[0]);
      auto &x533 = (x371[1]);
      auto &x535 = (x4[x533]);
      auto &x539 = (x7[x534]);
      std::cout << "(App access ((Id I2) (Id L3)))" << std::endl;
      auto &x542 = std::get<0>(*x540);
      auto &x541 = std::get<1>(*x540);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x549;
      x549 = std::make_pair(x541, x542);
      int x550 = reconstruct_L(x549);
      auto &x538 = std::get<0>(*x536);
      auto &x537 = std::get<1>(*x536);
      std::pair<std::vector<int>, std::vector<int>> x551;
      x551 = std::make_pair(x537, x538);
      int x552 = reconstruct_I(x551);
      // begin Array.init
      x544.clear();
      auto &x542 = std::get<0>(*x540);
      int x543 = (x542).size();
      x544.reserve(x543);
      auto &x542 = std::get<0>(*x540);
      int x543 = (x542).size();
      for (int x545 = 0; x545 < x543; x545++) {
        auto &x538 = std::get<0>(*x536);
        auto &x546 = (x538[x545]);
        auto &x542 = std::get<0>(*x540);
        auto &x547 = (x542[x545]);
        auto &x548 = (x547[x546]);
        x544.push_back(x548);
      }
      // end Array.init
      int x553 = (x544 == x372);
      if (x553) {
        x554 = 0;
      } else {
        x554 = 0;
      }
      for (auto x540 = x539.begin(); x540 != x539.end(); ++x540) {
        std::cout << "(App access ((Id I2) (Id L3)))" << std::endl;
        auto &x542 = std::get<0>(*x540);
        auto &x541 = std::get<1>(*x540);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x549;
        x549 = std::make_pair(x541, x542);
        int x550 = reconstruct_L(x549);
        auto &x538 = std::get<0>(*x536);
        auto &x537 = std::get<1>(*x536);
        std::pair<std::vector<int>, std::vector<int>> x551;
        x551 = std::make_pair(x537, x538);
        int x552 = reconstruct_I(x551);
        // begin Array.init
        x544.clear();
        auto &x542 = std::get<0>(*x540);
        int x543 = (x542).size();
        x544.reserve(x543);
        auto &x542 = std::get<0>(*x540);
        int x543 = (x542).size();
        for (int x545 = 0; x545 < x543; x545++) {
          auto &x538 = std::get<0>(*x536);
          auto &x546 = (x538[x545]);
          auto &x542 = std::get<0>(*x540);
          auto &x547 = (x542[x545]);
          auto &x548 = (x547[x546]);
          x544.push_back(x548);
        }
        // end Array.init
        int x553 = (x544 == x372);
        if (x553) {
          x554 = 0;
        } else {
          x554 = 0;
        }
      }
      for (auto x536 = x535.begin(); x536 != x535.end(); ++x536) {
        auto &x539 = (x7[x534]);
        std::cout << "(App access ((Id I2) (Id L3)))" << std::endl;
        auto &x542 = std::get<0>(*x540);
        auto &x541 = std::get<1>(*x540);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x549;
        x549 = std::make_pair(x541, x542);
        int x550 = reconstruct_L(x549);
        auto &x538 = std::get<0>(*x536);
        auto &x537 = std::get<1>(*x536);
        std::pair<std::vector<int>, std::vector<int>> x551;
        x551 = std::make_pair(x537, x538);
        int x552 = reconstruct_I(x551);
        // begin Array.init
        x544.clear();
        auto &x542 = std::get<0>(*x540);
        int x543 = (x542).size();
        x544.reserve(x543);
        auto &x542 = std::get<0>(*x540);
        int x543 = (x542).size();
        for (int x545 = 0; x545 < x543; x545++) {
          auto &x538 = std::get<0>(*x536);
          auto &x546 = (x538[x545]);
          auto &x542 = std::get<0>(*x540);
          auto &x547 = (x542[x545]);
          auto &x548 = (x547[x546]);
          x544.push_back(x548);
        }
        // end Array.init
        int x553 = (x544 == x372);
        if (x553) {
          x554 = 0;
        } else {
          x554 = 0;
        }
        for (auto x540 = x539.begin(); x540 != x539.end(); ++x540) {
          std::cout << "(App access ((Id I2) (Id L3)))" << std::endl;
          auto &x542 = std::get<0>(*x540);
          auto &x541 = std::get<1>(*x540);
          std::pair<std::vector<int>, std::vector<std::vector<int>>> x549;
          x549 = std::make_pair(x541, x542);
          int x550 = reconstruct_L(x549);
          auto &x538 = std::get<0>(*x536);
          auto &x537 = std::get<1>(*x536);
          std::pair<std::vector<int>, std::vector<int>> x551;
          x551 = std::make_pair(x537, x538);
          int x552 = reconstruct_I(x551);
          // begin Array.init
          x544.clear();
          auto &x542 = std::get<0>(*x540);
          int x543 = (x542).size();
          x544.reserve(x543);
          auto &x542 = std::get<0>(*x540);
          int x543 = (x542).size();
          for (int x545 = 0; x545 < x543; x545++) {
            auto &x538 = std::get<0>(*x536);
            auto &x546 = (x538[x545]);
            auto &x542 = std::get<0>(*x540);
            auto &x547 = (x542[x545]);
            auto &x548 = (x547[x546]);
            x544.push_back(x548);
          }
          // end Array.init
          int x553 = (x544 == x372);
          if (x553) {
            x554 = 0;
          } else {
            x554 = 0;
          }
        }
      }
      int x555 = (x371).size();
      int x556 = (x555 == 2);
      if (x556) {
        x557 = 0;
      } else {
        x557 = 0;
      }
      auto &x373 = (x371[0]);
      auto &x374 = (x7[x373]);
      std::cout << "(App count ((Id \"(%2==1)\") (Id L10)))" << std::endl;
      auto &x377 = std::get<0>(*x375);
      auto &x376 = std::get<1>(*x375);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x390;
      x390 = std::make_pair(x376, x377);
      int x391 = reconstruct_L(x390);
      // begin Array.init
      x379.clear();
      auto &x377 = std::get<0>(*x375);
      int x378 = (x377).size();
      x379.reserve(x378);
      auto &x377 = std::get<0>(*x375);
      int x378 = (x377).size();
      for (int x380 = 0; x380 < x378; x380++) {
        auto &x377 = std::get<0>(*x375);
        auto &x381 = (x377[x380]);
        // begin Array.fold
        x382 = 0;
        int x383 = (x381).size();
        for (int x384 = 0; x384 < x383; x384++) {
          int x386 = (x382 + 1);
          auto &x385 = (x381[x384]);
          int x387 = (x385 % 2);
          int x388 = (x387 == 1);
          if (x388) {
            x389 = x386;
          } else {
            x389 = x382;
          }
          x382 = x389;
        }
        // end Array.fold
        x379.push_back(x382);
      }
      // end Array.init
      int x392 = (x379 == x372);
      if (x392) {
        x393 = 0;
      } else {
        x393 = 0;
      }
      for (auto x375 = x374.begin(); x375 != x374.end(); ++x375) {
        std::cout << "(App count ((Id \"(%2==1)\") (Id L10)))" << std::endl;
        auto &x377 = std::get<0>(*x375);
        auto &x376 = std::get<1>(*x375);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x390;
        x390 = std::make_pair(x376, x377);
        int x391 = reconstruct_L(x390);
        // begin Array.init
        x379.clear();
        auto &x377 = std::get<0>(*x375);
        int x378 = (x377).size();
        x379.reserve(x378);
        auto &x377 = std::get<0>(*x375);
        int x378 = (x377).size();
        for (int x380 = 0; x380 < x378; x380++) {
          auto &x377 = std::get<0>(*x375);
          auto &x381 = (x377[x380]);
          // begin Array.fold
          x382 = 0;
          int x383 = (x381).size();
          for (int x384 = 0; x384 < x383; x384++) {
            int x386 = (x382 + 1);
            auto &x385 = (x381[x384]);
            int x387 = (x385 % 2);
            int x388 = (x387 == 1);
            if (x388) {
              x389 = x386;
            } else {
              x389 = x382;
            }
            x382 = x389;
          }
          // end Array.fold
          x379.push_back(x382);
        }
        // end Array.init
        int x392 = (x379 == x372);
        if (x392) {
          x393 = 0;
        } else {
          x393 = 0;
        }
      }
      auto &x394 = (x7[x373]);
      std::cout << "(App count ((Id \"(%2==0)\") (Id L9)))" << std::endl;
      auto &x397 = std::get<0>(*x395);
      auto &x396 = std::get<1>(*x395);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x410;
      x410 = std::make_pair(x396, x397);
      int x411 = reconstruct_L(x410);
      // begin Array.init
      x399.clear();
      auto &x397 = std::get<0>(*x395);
      int x398 = (x397).size();
      x399.reserve(x398);
      auto &x397 = std::get<0>(*x395);
      int x398 = (x397).size();
      for (int x400 = 0; x400 < x398; x400++) {
        auto &x397 = std::get<0>(*x395);
        auto &x401 = (x397[x400]);
        // begin Array.fold
        x402 = 0;
        int x403 = (x401).size();
        for (int x404 = 0; x404 < x403; x404++) {
          int x406 = (x402 + 1);
          auto &x405 = (x401[x404]);
          int x407 = (x405 % 2);
          int x408 = (x407 == 0);
          if (x408) {
            x409 = x406;
          } else {
            x409 = x402;
          }
          x402 = x409;
        }
        // end Array.fold
        x399.push_back(x402);
      }
      // end Array.init
      int x412 = (x399 == x372);
      if (x412) {
        x413 = 0;
      } else {
        x413 = 0;
      }
      for (auto x395 = x394.begin(); x395 != x394.end(); ++x395) {
        std::cout << "(App count ((Id \"(%2==0)\") (Id L9)))" << std::endl;
        auto &x397 = std::get<0>(*x395);
        auto &x396 = std::get<1>(*x395);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x410;
        x410 = std::make_pair(x396, x397);
        int x411 = reconstruct_L(x410);
        // begin Array.init
        x399.clear();
        auto &x397 = std::get<0>(*x395);
        int x398 = (x397).size();
        x399.reserve(x398);
        auto &x397 = std::get<0>(*x395);
        int x398 = (x397).size();
        for (int x400 = 0; x400 < x398; x400++) {
          auto &x397 = std::get<0>(*x395);
          auto &x401 = (x397[x400]);
          // begin Array.fold
          x402 = 0;
          int x403 = (x401).size();
          for (int x404 = 0; x404 < x403; x404++) {
            int x406 = (x402 + 1);
            auto &x405 = (x401[x404]);
            int x407 = (x405 % 2);
            int x408 = (x407 == 0);
            if (x408) {
              x409 = x406;
            } else {
              x409 = x402;
            }
            x402 = x409;
          }
          // end Array.fold
          x399.push_back(x402);
        }
        // end Array.init
        int x412 = (x399 == x372);
        if (x412) {
          x413 = 0;
        } else {
          x413 = 0;
        }
      }
      auto &x414 = (x7[x373]);
      std::cout << "(App count ((Id \"(<0)\") (Id L8)))" << std::endl;
      auto &x417 = std::get<0>(*x415);
      auto &x416 = std::get<1>(*x415);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x429;
      x429 = std::make_pair(x416, x417);
      int x430 = reconstruct_L(x429);
      // begin Array.init
      x419.clear();
      auto &x417 = std::get<0>(*x415);
      int x418 = (x417).size();
      x419.reserve(x418);
      auto &x417 = std::get<0>(*x415);
      int x418 = (x417).size();
      for (int x420 = 0; x420 < x418; x420++) {
        auto &x417 = std::get<0>(*x415);
        auto &x421 = (x417[x420]);
        // begin Array.fold
        x422 = 0;
        int x423 = (x421).size();
        for (int x424 = 0; x424 < x423; x424++) {
          int x426 = (x422 + 1);
          auto &x425 = (x421[x424]);
          int x427 = (x425 < 0);
          if (x427) {
            x428 = x426;
          } else {
            x428 = x422;
          }
          x422 = x428;
        }
        // end Array.fold
        x419.push_back(x422);
      }
      // end Array.init
      int x431 = (x419 == x372);
      if (x431) {
        x432 = 0;
      } else {
        x432 = 0;
      }
      for (auto x415 = x414.begin(); x415 != x414.end(); ++x415) {
        std::cout << "(App count ((Id \"(<0)\") (Id L8)))" << std::endl;
        auto &x417 = std::get<0>(*x415);
        auto &x416 = std::get<1>(*x415);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x429;
        x429 = std::make_pair(x416, x417);
        int x430 = reconstruct_L(x429);
        // begin Array.init
        x419.clear();
        auto &x417 = std::get<0>(*x415);
        int x418 = (x417).size();
        x419.reserve(x418);
        auto &x417 = std::get<0>(*x415);
        int x418 = (x417).size();
        for (int x420 = 0; x420 < x418; x420++) {
          auto &x417 = std::get<0>(*x415);
          auto &x421 = (x417[x420]);
          // begin Array.fold
          x422 = 0;
          int x423 = (x421).size();
          for (int x424 = 0; x424 < x423; x424++) {
            int x426 = (x422 + 1);
            auto &x425 = (x421[x424]);
            int x427 = (x425 < 0);
            if (x427) {
              x428 = x426;
            } else {
              x428 = x422;
            }
            x422 = x428;
          }
          // end Array.fold
          x419.push_back(x422);
        }
        // end Array.init
        int x431 = (x419 == x372);
        if (x431) {
          x432 = 0;
        } else {
          x432 = 0;
        }
      }
      auto &x433 = (x7[x373]);
      std::cout << "(App count ((Id \"(>0)\") (Id L7)))" << std::endl;
      auto &x436 = std::get<0>(*x434);
      auto &x435 = std::get<1>(*x434);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x448;
      x448 = std::make_pair(x435, x436);
      int x449 = reconstruct_L(x448);
      // begin Array.init
      x438.clear();
      auto &x436 = std::get<0>(*x434);
      int x437 = (x436).size();
      x438.reserve(x437);
      auto &x436 = std::get<0>(*x434);
      int x437 = (x436).size();
      for (int x439 = 0; x439 < x437; x439++) {
        auto &x436 = std::get<0>(*x434);
        auto &x440 = (x436[x439]);
        // begin Array.fold
        x441 = 0;
        int x442 = (x440).size();
        for (int x443 = 0; x443 < x442; x443++) {
          int x445 = (x441 + 1);
          auto &x444 = (x440[x443]);
          int x446 = (x444 > 0);
          if (x446) {
            x447 = x445;
          } else {
            x447 = x441;
          }
          x441 = x447;
        }
        // end Array.fold
        x438.push_back(x441);
      }
      // end Array.init
      int x450 = (x438 == x372);
      if (x450) {
        x451 = 0;
      } else {
        x451 = 0;
      }
      for (auto x434 = x433.begin(); x434 != x433.end(); ++x434) {
        std::cout << "(App count ((Id \"(>0)\") (Id L7)))" << std::endl;
        auto &x436 = std::get<0>(*x434);
        auto &x435 = std::get<1>(*x434);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x448;
        x448 = std::make_pair(x435, x436);
        int x449 = reconstruct_L(x448);
        // begin Array.init
        x438.clear();
        auto &x436 = std::get<0>(*x434);
        int x437 = (x436).size();
        x438.reserve(x437);
        auto &x436 = std::get<0>(*x434);
        int x437 = (x436).size();
        for (int x439 = 0; x439 < x437; x439++) {
          auto &x436 = std::get<0>(*x434);
          auto &x440 = (x436[x439]);
          // begin Array.fold
          x441 = 0;
          int x442 = (x440).size();
          for (int x443 = 0; x443 < x442; x443++) {
            int x445 = (x441 + 1);
            auto &x444 = (x440[x443]);
            int x446 = (x444 > 0);
            if (x446) {
              x447 = x445;
            } else {
              x447 = x441;
            }
            x441 = x447;
          }
          // end Array.fold
          x438.push_back(x441);
        }
        // end Array.init
        int x450 = (x438 == x372);
        if (x450) {
          x451 = 0;
        } else {
          x451 = 0;
        }
      }
      auto &x452 = (x7[x373]);
      std::cout << "(App sum ((Id L6)))" << std::endl;
      auto &x455 = std::get<0>(*x453);
      auto &x454 = std::get<1>(*x453);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x465;
      x465 = std::make_pair(x454, x455);
      int x466 = reconstruct_L(x465);
      // begin Array.init
      x457.clear();
      auto &x455 = std::get<0>(*x453);
      int x456 = (x455).size();
      x457.reserve(x456);
      auto &x455 = std::get<0>(*x453);
      int x456 = (x455).size();
      for (int x458 = 0; x458 < x456; x458++) {
        auto &x455 = std::get<0>(*x453);
        auto &x459 = (x455[x458]);
        // begin Array.fold
        x460 = 0;
        int x461 = (x459).size();
        for (int x462 = 0; x462 < x461; x462++) {
          auto &x463 = (x459[x462]);
          int x464 = (x460 + x463);
          x460 = x464;
        }
        // end Array.fold
        x457.push_back(x460);
      }
      // end Array.init
      int x467 = (x457 == x372);
      if (x467) {
        x468 = 0;
      } else {
        x468 = 0;
      }
      for (auto x453 = x452.begin(); x453 != x452.end(); ++x453) {
        std::cout << "(App sum ((Id L6)))" << std::endl;
        auto &x455 = std::get<0>(*x453);
        auto &x454 = std::get<1>(*x453);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x465;
        x465 = std::make_pair(x454, x455);
        int x466 = reconstruct_L(x465);
        // begin Array.init
        x457.clear();
        auto &x455 = std::get<0>(*x453);
        int x456 = (x455).size();
        x457.reserve(x456);
        auto &x455 = std::get<0>(*x453);
        int x456 = (x455).size();
        for (int x458 = 0; x458 < x456; x458++) {
          auto &x455 = std::get<0>(*x453);
          auto &x459 = (x455[x458]);
          // begin Array.fold
          x460 = 0;
          int x461 = (x459).size();
          for (int x462 = 0; x462 < x461; x462++) {
            auto &x463 = (x459[x462]);
            int x464 = (x460 + x463);
            x460 = x464;
          }
          // end Array.fold
          x457.push_back(x460);
        }
        // end Array.init
        int x467 = (x457 == x372);
        if (x467) {
          x468 = 0;
        } else {
          x468 = 0;
        }
      }
      auto &x469 = (x7[x373]);
      std::cout << "(App maximum ((Id L5)))" << std::endl;
      auto &x472 = std::get<0>(*x470);
      auto &x471 = std::get<1>(*x470);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x483;
      x483 = std::make_pair(x471, x472);
      int x484 = reconstruct_L(x483);
      // begin Array.init
      x474.clear();
      auto &x472 = std::get<0>(*x470);
      int x473 = (x472).size();
      x474.reserve(x473);
      auto &x472 = std::get<0>(*x470);
      int x473 = (x472).size();
      for (int x475 = 0; x475 < x473; x475++) {
        auto &x472 = std::get<0>(*x470);
        auto &x476 = (x472[x475]);
        // begin Array.fold
        x477 = -2147483648;
        int x478 = (x476).size();
        for (int x479 = 0; x479 < x478; x479++) {
          auto &x480 = (x476[x479]);
          auto &x480 = (x476[x479]);
          int x481 = (x480 > x477);
          if (x481) {
            x482 = x480;
          } else {
            x482 = x477;
          }
          x477 = x482;
        }
        // end Array.fold
        x474.push_back(x477);
      }
      // end Array.init
      int x485 = (x474 == x372);
      if (x485) {
        x486 = 0;
      } else {
        x486 = 0;
      }
      for (auto x470 = x469.begin(); x470 != x469.end(); ++x470) {
        std::cout << "(App maximum ((Id L5)))" << std::endl;
        auto &x472 = std::get<0>(*x470);
        auto &x471 = std::get<1>(*x470);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x483;
        x483 = std::make_pair(x471, x472);
        int x484 = reconstruct_L(x483);
        // begin Array.init
        x474.clear();
        auto &x472 = std::get<0>(*x470);
        int x473 = (x472).size();
        x474.reserve(x473);
        auto &x472 = std::get<0>(*x470);
        int x473 = (x472).size();
        for (int x475 = 0; x475 < x473; x475++) {
          auto &x472 = std::get<0>(*x470);
          auto &x476 = (x472[x475]);
          // begin Array.fold
          x477 = -2147483648;
          int x478 = (x476).size();
          for (int x479 = 0; x479 < x478; x479++) {
            auto &x480 = (x476[x479]);
            auto &x480 = (x476[x479]);
            int x481 = (x480 > x477);
            if (x481) {
              x482 = x480;
            } else {
              x482 = x477;
            }
            x477 = x482;
          }
          // end Array.fold
          x474.push_back(x477);
        }
        // end Array.init
        int x485 = (x474 == x372);
        if (x485) {
          x486 = 0;
        } else {
          x486 = 0;
        }
      }
      auto &x487 = (x7[x373]);
      std::cout << "(App minimum ((Id L4)))" << std::endl;
      auto &x490 = std::get<0>(*x488);
      auto &x489 = std::get<1>(*x488);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x501;
      x501 = std::make_pair(x489, x490);
      int x502 = reconstruct_L(x501);
      // begin Array.init
      x492.clear();
      auto &x490 = std::get<0>(*x488);
      int x491 = (x490).size();
      x492.reserve(x491);
      auto &x490 = std::get<0>(*x488);
      int x491 = (x490).size();
      for (int x493 = 0; x493 < x491; x493++) {
        auto &x490 = std::get<0>(*x488);
        auto &x494 = (x490[x493]);
        // begin Array.fold
        x495 = 2147483647;
        int x496 = (x494).size();
        for (int x497 = 0; x497 < x496; x497++) {
          auto &x498 = (x494[x497]);
          auto &x498 = (x494[x497]);
          int x499 = (x498 < x495);
          if (x499) {
            x500 = x498;
          } else {
            x500 = x495;
          }
          x495 = x500;
        }
        // end Array.fold
        x492.push_back(x495);
      }
      // end Array.init
      int x503 = (x492 == x372);
      if (x503) {
        x504 = 0;
      } else {
        x504 = 0;
      }
      for (auto x488 = x487.begin(); x488 != x487.end(); ++x488) {
        std::cout << "(App minimum ((Id L4)))" << std::endl;
        auto &x490 = std::get<0>(*x488);
        auto &x489 = std::get<1>(*x488);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x501;
        x501 = std::make_pair(x489, x490);
        int x502 = reconstruct_L(x501);
        // begin Array.init
        x492.clear();
        auto &x490 = std::get<0>(*x488);
        int x491 = (x490).size();
        x492.reserve(x491);
        auto &x490 = std::get<0>(*x488);
        int x491 = (x490).size();
        for (int x493 = 0; x493 < x491; x493++) {
          auto &x490 = std::get<0>(*x488);
          auto &x494 = (x490[x493]);
          // begin Array.fold
          x495 = 2147483647;
          int x496 = (x494).size();
          for (int x497 = 0; x497 < x496; x497++) {
            auto &x498 = (x494[x497]);
            auto &x498 = (x494[x497]);
            int x499 = (x498 < x495);
            if (x499) {
              x500 = x498;
            } else {
              x500 = x495;
            }
            x495 = x500;
          }
          // end Array.fold
          x492.push_back(x495);
        }
        // end Array.init
        int x503 = (x492 == x372);
        if (x503) {
          x504 = 0;
        } else {
          x504 = 0;
        }
      }
      auto &x505 = (x7[x373]);
      std::cout << "(App last ((Id L1)))" << std::endl;
      auto &x508 = std::get<0>(*x506);
      auto &x507 = std::get<1>(*x506);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x516;
      x516 = std::make_pair(x507, x508);
      int x517 = reconstruct_L(x516);
      auto &x508 = std::get<0>(*x506);
      // begin Array.init
      x510.clear();
      int x509 = (x508).size();
      x510.reserve(x509);
      int x509 = (x508).size();
      for (int x511 = 0; x511 < x509; x511++) {
        auto &x512 = (x508[x511]);
        int x513 = (x512).size();
        int x514 = (x513 - 1);
        auto &x515 = (x512[x514]);
        x510.push_back(x515);
      }
      // end Array.init
      int x518 = (x510 == x372);
      if (x518) {
        x519 = 0;
      } else {
        x519 = 0;
      }
      for (auto x506 = x505.begin(); x506 != x505.end(); ++x506) {
        std::cout << "(App last ((Id L1)))" << std::endl;
        auto &x508 = std::get<0>(*x506);
        auto &x507 = std::get<1>(*x506);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x516;
        x516 = std::make_pair(x507, x508);
        int x517 = reconstruct_L(x516);
        auto &x508 = std::get<0>(*x506);
        // begin Array.init
        x510.clear();
        int x509 = (x508).size();
        x510.reserve(x509);
        int x509 = (x508).size();
        for (int x511 = 0; x511 < x509; x511++) {
          auto &x512 = (x508[x511]);
          int x513 = (x512).size();
          int x514 = (x513 - 1);
          auto &x515 = (x512[x514]);
          x510.push_back(x515);
        }
        // end Array.init
        int x518 = (x510 == x372);
        if (x518) {
          x519 = 0;
        } else {
          x519 = 0;
        }
      }
      auto &x520 = (x7[x373]);
      std::cout << "(App head ((Id L0)))" << std::endl;
      auto &x523 = std::get<0>(*x521);
      auto &x522 = std::get<1>(*x521);
      std::pair<std::vector<int>, std::vector<std::vector<int>>> x529;
      x529 = std::make_pair(x522, x523);
      int x530 = reconstruct_L(x529);
      // begin Array.init
      x525.clear();
      auto &x523 = std::get<0>(*x521);
      int x524 = (x523).size();
      x525.reserve(x524);
      auto &x523 = std::get<0>(*x521);
      int x524 = (x523).size();
      for (int x526 = 0; x526 < x524; x526++) {
        auto &x523 = std::get<0>(*x521);
        auto &x527 = (x523[x526]);
        auto &x528 = (x527[0]);
        x525.push_back(x528);
      }
      // end Array.init
      int x531 = (x525 == x372);
      if (x531) {
        x532 = 0;
      } else {
        x532 = 0;
      }
      for (auto x521 = x520.begin(); x521 != x520.end(); ++x521) {
        std::cout << "(App head ((Id L0)))" << std::endl;
        auto &x523 = std::get<0>(*x521);
        auto &x522 = std::get<1>(*x521);
        std::pair<std::vector<int>, std::vector<std::vector<int>>> x529;
        x529 = std::make_pair(x522, x523);
        int x530 = reconstruct_L(x529);
        // begin Array.init
        x525.clear();
        auto &x523 = std::get<0>(*x521);
        int x524 = (x523).size();
        x525.reserve(x524);
        auto &x523 = std::get<0>(*x521);
        int x524 = (x523).size();
        for (int x526 = 0; x526 < x524; x526++) {
          auto &x523 = std::get<0>(*x521);
          auto &x527 = (x523[x526]);
          auto &x528 = (x527[0]);
          x525.push_back(x528);
        }
        // end Array.init
        int x531 = (x525 == x372);
        if (x531) {
          x532 = 0;
        } else {
          x532 = 0;
        }
      }
      int x558 = (x371).size();
      int x559 = (x558 == 1);
      if (x559) {
        x560 = 0;
      } else {
        x560 = x557;
      }
      return x560;
    }
 |}]
