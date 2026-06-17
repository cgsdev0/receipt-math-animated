open! Core

let pow x y = Float.(x ** y)

module Rgba = struct
  type t =
    { r : float
    ; g : float
    ; b : float
    ; alpha : float
    }
  [@@deriving sexp_of]
end

module Rgba_linear = struct
  type t =
    { r : float
    ; g : float
    ; b : float
    ; alpha : float
    }
  [@@deriving sexp_of]

  let of_rgba { Rgba.r; g; b; alpha } =
    let f c =
      let c_abs = Float.abs c in
      if Float.(c_abs < 0.04045)
      then c /. 12.92
      else (
        let sign =
          match Float.sign_or_nan c with
          | Neg -> -1.0
          | Zero | Pos | Nan -> 1.0
        in
        sign *. pow ((c_abs +. 0.055) /. 1.055) 2.4)
    in
    { r = f r; g = f g; b = f b; alpha }
  ;;

  let to_rgba { r; g; b; alpha } =
    let f c =
      let c_abs = Float.abs c in
      if Float.(c_abs > 0.0031308)
      then (
        let sign =
          match Float.sign_or_nan c with
          | Neg -> -1.0
          | Zero | Pos | Nan -> 1.0
        in
        sign *. ((1.055 *. pow c_abs (1.0 /. 2.4)) -. 0.055))
      else c *. 12.92
    in
    { Rgba.r = f r; g = f g; b = f b; alpha }
  ;;
end

module Oklab = struct
  type t =
    { l : float
    ; a : float
    ; b : float
    ; alpha : float
    }
  [@@deriving sexp_of]

  let cbrt x = Float.(x ** (1.0 / 3.0))

  let of_rgba_linear { Rgba_linear.r; g; b; alpha } =
    let l = cbrt ((0.4122214708 *. r) +. (0.5363325363 *. g) +. (0.0514459929 *. b))
    and m = cbrt ((0.2119034982 *. r) +. (0.6806995451 *. g) +. (0.1073969566 *. b))
    and s = cbrt ((0.0883024619 *. r) +. (0.2817188376 *. g) +. (0.6299787005 *. b)) in
    { l = (0.2104542553 *. l) +. (0.793617785 *. m) -. (0.0040720468 *. s)
    ; a = (1.9779984951 *. l) -. (2.428592205 *. m) +. (0.4505937099 *. s)
    ; b = (0.0259040371 *. l) +. (0.7827717662 *. m) -. (0.808675766 *. s)
    ; alpha
    }
  ;;

  let to_rgba_linear { l; a; b; alpha } =
    let l = pow (l +. (0.3963377774 *. a) +. (0.2158037573 *. b)) 3.0
    and m = pow (l -. (0.1055613458 *. a) -. (0.0638541728 *. b)) 3.0
    and s = pow (l -. (0.0894841775 *. a) -. (1.291485548 *. b)) 3.0 in
    { Rgba_linear.r = (4.0767416621 *. l) -. (3.3077115913 *. m) +. (0.2309699292 *. s)
    ; g = (-1.2684380046 *. l) +. (2.6097574011 *. m) -. (0.3413193965 *. s)
    ; b = (-0.0041960863 *. l) -. (0.7034186147 *. m) +. (1.707614701 *. s)
    ; alpha
    }
  ;;

  let inside_rgb t =
    let { Rgba.r; g; b; _ } = Rgba_linear.to_rgba (to_rgba_linear t) in
    Float.(r <= 1.0 && g <= 1.0 && b <= 1.0)
  ;;

  module Lch = struct
    type t =
      { l : float
      ; c : float
      ; h : float
      ; alpha : float
      }

    let create ~l ~c ~h ?(alpha = 1.0) () = { l; c; h; alpha }

    let of_lab { l; a; b; alpha } =
      let c = Float.(sqrt ((a ** 2.0) + (b ** 2.0))) in
      let h = Float.atan2 b a in
      { l; c; h; alpha }
    ;;

    let to_lab { l; c; h; alpha } =
      let a = c *. Float.cos h in
      let b = c *. Float.sin h in
      { l; a; b; alpha }
    ;;

    let lightness { l; _ } = l
    let chroma { c; _ } = c
    let hue { h; _ } = h
    let alpha { alpha; _ } = alpha
    let rotate_hue t ~rad = { t with h = t.h +. rad }
    let set_hue t ~rad = { t with h = rad }
    let set_chroma t c = { t with c }
    let set_lightness t l = { t with l }
    let set_alpha t alpha = { t with alpha }
  end
end

type t = Oklab.t [@@deriving sexp_of]

module Lch = Oklab.Lch

let create ~l ~a ~b ?(alpha = 1.0) () = { Oklab.l; a; b; alpha }

let of_rgb ~r ~g ~b ?(alpha = 1.0) () =
  let c x =
    match Float.clamp ~min:0.0 ~max:1.0 x with
    | Ok x -> x
    | Error _ -> 0.0
  in
  let r = c r
  and g = c g
  and b = c b in
  let rgba = { Rgba.r; g; b; alpha } in
  let rgba_linear = Rgba_linear.of_rgba rgba in
  Oklab.of_rgba_linear rgba_linear
;;

let byte_to_float v = Int.to_float v /. 255.0

let of_rgb' ~r ~g ~b ?(alpha = 1.0) () =
  let r = byte_to_float r in
  let g = byte_to_float g in
  let b = byte_to_float b in
  of_rgb ~r ~g ~b ~alpha ()
;;

let of_rgb_hex s =
  let s = String.chop_prefix_if_exists s ~prefix:"#" in
  let f a b = Int.of_string ("0x" ^ Char.to_string a ^ Char.to_string b) in
  match String.to_list s with
  | [ r; g; b ] ->
    let r = f r r in
    let g = f g g in
    let b = f b b in
    of_rgb' ~r ~g ~b ~alpha:1.0 ()
  | [ r; g; b; a ] ->
    let r = f r r in
    let g = f g g in
    let b = f b b in
    let alpha = byte_to_float (f a a) in
    of_rgb' ~r ~g ~b ~alpha ()
  | [ r1; r2; g1; g2; b1; b2 ] ->
    let r = f r1 r2 in
    let g = f g1 g2 in
    let b = f b1 b2 in
    of_rgb' ~r ~g ~b ~alpha:1.0 ()
  | [ r1; r2; g1; g2; b1; b2; a1; a2 ] ->
    let r = f r1 r2 in
    let g = f g1 g2 in
    let b = f b1 b2 in
    let alpha = byte_to_float (f a1 a2) in
    of_rgb' ~r ~g ~b ~alpha ()
  | _ ->
    let error = Printf.sprintf "invalid hex color: %s\n" s in
    Out_channel.output_string Out_channel.stderr error;
    Out_channel.flush Out_channel.stderr;
    of_rgb ~r:1.0 ~g:0.0 ~b:0.0 ~alpha:1.0 ()
;;

let inside_rgb = Oklab.inside_rgb

let float_to_byte (v : float) : int =
  let result = Float.iround_nearest_exn (v *. 255.0) in
  Int.clamp_exn result ~min:0 ~max:255
;;

let to_string_css { Oklab.l; a; b; alpha } =
  if Float.(alpha >= 1.0)
  then Printf.sprintf "oklab(%.6f %.6f %.6f)" l a b
  else Printf.sprintf "oklab(%.6f %.6f %.6f / %.6f)" l a b alpha
;;

let to_rgb (t : t) =
  let { Rgba.r; g; b; alpha = _ } = t |> Oklab.to_rgba_linear |> Rgba_linear.to_rgba in
  let r, g, b = float_to_byte r, float_to_byte g, float_to_byte b in
  r, g, b
;;

let to_string_hex (t : t) =
  let { Rgba.r; g; b; alpha } = t |> Oklab.to_rgba_linear |> Rgba_linear.to_rgba in
  let r, g, b, a =
    float_to_byte r, float_to_byte g, float_to_byte b, float_to_byte alpha
  in
  (* Avoid overflowing 31 bits to support, e.g., the Wasm backend *)
  let result_rg = (r lsl 8) lor g in
  let result_ba = (b lsl 8) lor a in
  Printf.sprintf "#%04x%04x" result_rg result_ba
;;

let flerp a b amt = ((1.0 -. amt) *. a) +. (amt *. b)

let lerp (x : t) (y : t) amt : t =
  let l = flerp x.l y.l amt in
  let a = flerp x.a y.a amt in
  let b = flerp x.b y.b amt in
  let alpha = flerp x.alpha y.alpha amt in
  { l; a; b; alpha }
;;

let composite
  ~under:{ Oklab.l = l_b; a = a_b; b = b_b; alpha = alpha_b }
  ~over:{ Oklab.l = l_a; a = a_a; b = b_a; alpha = alpha_a }
  =
  let alpha_out = alpha_a +. (alpha_b *. (1.0 -. alpha_a)) in
  let f c_a c_b =
    ((c_a *. alpha_a) +. (c_b *. alpha_b *. (1.0 -. alpha_a))) /. alpha_out
  in
  { Oklab.l = f l_a l_b; a = f a_a a_b; b = f b_a b_b; alpha = alpha_out }
;;

let lightness (t : Oklab.t) = t.l
let set_lightness (t : Oklab.t) l = { t with l }
let alpha (t : Oklab.t) = t.alpha

let set_alpha (t : Oklab.t) ~alpha =
  let alpha = Float.clamp_exn alpha ~min:0.0 ~max:1.0 in
  { t with alpha }
;;
