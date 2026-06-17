open! Core

type t [@@deriving sexp_of, equal]

val random : unit -> t
val scale : t -> int
val gradient : t -> [ `linear | `square | `sqrt | `sin | `cos ]
val get_start_color : t -> Oklab.t
val get_end_color : t -> Oklab.t
