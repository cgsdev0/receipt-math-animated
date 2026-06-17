open! Core

(** represents a point in the oklab color space *)
type t [@@deriving sexp_of]

(** constructors *)

(** Create using direct Oklab values. *)
val create : l:float -> a:float -> b:float -> ?alpha:float -> unit -> t

(** Each of [r], [g], [b], and [alpha] must be in the range 0 to 1 *)
val of_rgb : r:float -> g:float -> b:float -> ?alpha:float -> unit -> t

(** Each of [r], [g], [b], and [alpha] must be in the range 0 to 1 *)
val to_rgb : t -> int * int * int

(** Each of [r], [g], [b] must be in the range 0 to 255. [alpha] must be in the range 0 to
    1 *)
val of_rgb' : r:int -> g:int -> b:int -> ?alpha:float -> unit -> t

(** Parses css hex rgb[a] colors *)
val of_rgb_hex : string -> t

(** Formats the color for use in a css color, in rgb notation *)
val to_string_css : t -> string

(** Formats the color for use in a css color, in hex notation *)
val to_string_hex : t -> string

(* getters *)
val lightness : t -> float
val alpha : t -> float

(* setters *)
val set_lightness : t -> float -> t
val set_alpha : t -> alpha:float -> t

(** [lerp start end mult] produces a color that is between [start] and [end] as determined
    by [mult], which must be in the range 0 to 1 *)
val lerp : t -> t -> float -> t

(** Builds a color that is the result of compositing [over] on top of [under] taking
    trasnparencies into account. *)
val composite : under:t -> over:t -> t

val inside_rgb : t -> bool

module Lch : sig
  type lab := t
  type t

  (** Create using direct Oklch values. *)
  val create : l:float -> c:float -> h:float -> ?alpha:float -> unit -> t

  (* converters *)
  val of_lab : lab -> t
  val to_lab : t -> lab

  (* getters *)
  val lightness : t -> float
  val hue : t -> float
  val chroma : t -> float
  val alpha : t -> float

  (* setters *)
  val set_alpha : t -> float -> t
  val set_lightness : t -> float -> t
  val set_hue : t -> rad:float -> t
  val set_chroma : t -> float -> t
  val rotate_hue : t -> rad:float -> t
end
