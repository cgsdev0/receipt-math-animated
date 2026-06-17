open! Core

type t =
  | X
  | Y
  | C of int
  | Xor of t * t
  | And of t * t
  | Or of t * t
  | MirrorX of t
  | MirrorY of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Mod of t * t
[@@deriving quickcheck, sexp]

val generate : unit -> t
val dimension : int
val eval : x:int -> y:int -> int -> t -> int

module For_testing : sig
  val simplify : t -> t
  val stats : t -> int * bool * bool
end
