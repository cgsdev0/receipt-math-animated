open! Core

type t [@@deriving sexp_of, equal]

val random : unit -> t
val scale : t -> int

(* Selects which expression evaluator the pipeline uses: [`int] runs [Expr_int] (modular
   byte arithmetic), [`float] runs [Expr_float] (continuous unit-interval arithmetic). *)
val numeric : t -> [ `int | `float ]
val gradient : t -> [ `linear | `square | `sqrt | `sin | `cos ]
val get_start_color : t -> Oklab.t
val get_end_color : t -> Oklab.t
