open! Core

type t = A of int | L of int list [@@deriving compare, hash, sexp]
