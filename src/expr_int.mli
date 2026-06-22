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

(* Parse the syntax produced by [to_string]. [of_string] reports lexing/parsing
   failures as [Error]; [of_string_exn] raises on them. *)
val of_string : string -> t Or_error.t
val of_string_exn : string -> t

val generate : unit -> t
val dimension : int
val eval : x:int -> y:int -> time:float -> int -> t -> int
val to_frag : int -> t -> string

module For_testing : sig
  val simplify : t -> t
  val stats : t -> int * bool * bool * bool
end
