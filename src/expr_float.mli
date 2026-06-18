open! Core

(* The floating-point cousin of [Expr_int]. [X] and [Y] are normalised coordinates in
   [0, 1], and every operator maps [0, 1] inputs back into [0, 1] continuously (no sharp
   discontinuities), so [eval] always returns a value in [0, 1]. *)
type t =
  | T
  | X
  | Y
  | C of float
  | Xor of t * t
  | And of t * t
  | Or of t * t
  | MirrorX of t
  | MirrorY of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Mod of t * t
  | Sin of t
  | Cos of t
[@@deriving quickcheck, sexp]

val generate : unit -> t
val eval : x:float -> y:float -> time:float -> t -> float

module For_testing : sig
  val simplify : t -> t
  val stats : t -> int * bool * bool * bool
end
