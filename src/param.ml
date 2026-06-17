open! Core

module type Bounds = sig
  val low : float
  val high : float
end

module Scale = struct
  include Int

  let quickcheck_generator = Base_quickcheck.Generator.int_inclusive 0 2
end

module Bound_float (B : Bounds) = struct
  include Float

  let quickcheck_generator = Base_quickcheck.Generator.float_inclusive B.low B.high
end

module Z_to_one = Bound_float (struct
    let low = 0.0
    let high = 1.0
  end)

module Lightness = Bound_float (struct
    let low = 0.0
    let high = 1.0
  end)

module Lightness_delta = Bound_float (struct
    let low = 0.25
    let high = 0.75
  end)

module Chroma = Bound_float (struct
    let low = 0.0
    let high = 0.3
  end)

module Chroma_delta = Bound_float (struct
    let low = 0.1
    let high = 0.2
  end)

module Hue = Bound_float (struct
    let low = 0.0
    let high = 360.0
  end)

module Hue_delta = Bound_float (struct
    let low = 0.0
    let high = 360.0
  end)

type t =
  { scale : Scale.t
  ; gradient : [ `linear | `square | `sqrt | `sin | `cos ]
  ; lightness : Lightness.t
  ; lightness_delta : Lightness_delta.t
  ; chroma : Chroma.t
  ; chroma_delta : Chroma_delta.t
  ; hue : Hue.t
  ; hue_delta : Hue_delta.t
  }
[@@deriving quickcheck, sexp_of, equal]

let scale { scale; _ } = scale
let gradient { gradient; _ } = gradient

let get_start_color' { lightness; chroma; hue; _ } =
  Oklab.Lch.create ~l:lightness ~c:chroma ~h:hue ()
;;

let get_end_color' { lightness; lightness_delta; chroma; chroma_delta; hue; hue_delta; _ }
  =
  let open Float in
  let l = (lightness + lightness_delta) % 1.0 in
  let c = (chroma + chroma_delta) % 0.3 in
  let h = (hue + hue_delta) % 360.0 in
  Oklab.Lch.create ~l ~c ~h ()
;;

let get_start_color params =
  let a = get_start_color' params in
  let b = get_end_color' params in
  Oklab.Lch.to_lab
    (if Float.(Oklab.Lch.lightness a > Oklab.Lch.lightness b) then b else a)
;;

let get_end_color params =
  let a = get_start_color' params in
  let b = get_end_color' params in
  Oklab.Lch.to_lab
    (if Float.(Oklab.Lch.lightness a > Oklab.Lch.lightness b) then a else b)
;;
