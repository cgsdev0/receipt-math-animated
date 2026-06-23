open! Core

type t =
  | T
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
[@@deriving quickcheck, sexp, compare, equal]

(* Render [t] in the C-like concrete syntax: infix operators ([|] [^] [&] [+]
   [-] [*] [%]) for the binary nodes, [mirrorX(_)]/[mirrorY(_)] for the mirrors,
   [x]/[y] for the coordinates, and decimal literals for [C]. The result parses
   back to a structurally identical tree via [of_string]. *)
val to_string : t -> string

val generate : unit -> t
val dimension : int
val eval : x:int -> y:int -> time:float -> int -> t -> int
val to_frag : int -> t -> string

module For_testing : sig
  val simplify : t -> t
  val stats : t -> int * bool * bool * bool
end
